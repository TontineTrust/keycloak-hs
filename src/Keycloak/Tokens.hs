{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Authentication with Keycloak is based on [JWTs](https://jwt.io/).
-- This module helps you retrieve tokens from Keycloak, and use them to authenticate your users.
-- In Keycloak, you need to configure a realm, a client and a user.
--
-- Users can also have additional attributes.
-- To see them in the Token, you need to add "protocol mappers" in the Client, that will copy the User attribute in the Token.
--
-- The example below retrieves a User token using Login/password, verifies it, and extract all the user details from it.
--
-- @
-- main :: IO ()
-- main = do
--
--   --configure Keycloak with the adapter config file. You can retrieve this file in your Client/Installation tab (JSON format).
--   --This function will also get the signing keys from Keycloak, so make sure that Keycloak is on and configured!
--   kcConfig <- configureKeycloak "keycloak.json"
--
--   void $ flip runKeycloak kcConfig $ do
--
--     -- Get a JWT from Keycloak. A JWT can then be used to authenticate yourself with an application.
--     jwt <- getJWT "demo" "demo"
--     liftIO $ putStrLn $ "Got JWT: \n" <> (show jwt) ++ "\n\n"
--
--     -- Retrieve the claims contained in the JWT.
--     claims <- verifyJWT jwt
--     liftIO $ putStrLn $ "Claims decoded from Token: \n" <> (show claims) ++ "\n\n"
--
--     -- get the user from the claim
--     let user = getClaimsUser claims
--     liftIO $ putStrLn $ "User decoded from claims: \n" <> (show user) ++ "\n\n"
-- @
module Keycloak.Tokens where

import Control.Lens hiding ((.=))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.Time (MonadTime)
import Crypto.JWT as JWT
import Data.Aeson as JSON
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Conversions
import Data.Text as T hiding (head, map, show, tail)
import GHC.Generics (Generic)
import Keycloak.Types
import Keycloak.Utils
import Network.Wreq as W hiding (statusCode)

-- | Keycloak-specific claims that extend the standard JWT ClaimsSet.
-- This type wraps ClaimsSet and adds Keycloak-specific fields.
data KeycloakClaims = KeycloakClaims
  { jwtClaims :: ClaimsSet,
    preferredUsername :: Maybe Text,
    givenName :: Maybe Text,
    familyName :: Maybe Text,
    emailClaim :: Maybe Text,
    additionalClaims :: Maybe (Map Text Value)
  }
  deriving stock (Generic, Show)

instance HasClaimsSet KeycloakClaims where
  claimsSet f s = (\a' -> s {jwtClaims = a'}) <$> f (jwtClaims s)

instance FromJSON KeycloakClaims where
  parseJSON = withObject "KeycloakClaims" $ \o -> do
    claims <- parseJSON (Object o)
    let attrsMap = Map.fromList [(toText k, v) | (k, v) <- KM.toList o]
    prefUsername <- o .:? "preferred_username"
    gName <- o .:? "given_name"
    fName <- o .:? "family_name"
    email <- o .:? "email"
    return $ KeycloakClaims claims prefUsername gName fName email (Just attrsMap)

instance ToJSON KeycloakClaims where
  toJSON kc = case toJSON (jwtClaims kc) of
    Object o ->
      Object $
        KM.insert "preferred_username" (toJSON $ preferredUsername kc) $
          KM.insert "given_name" (toJSON $ givenName kc) $
            KM.insert "family_name" (toJSON $ familyName kc) $
              KM.insert "email" (toJSON $ emailClaim kc) o
    a -> a

-- | Retrieve the user's token. This token can be used to authenticate the user.
-- This token can be also used for every other Keycloak calls.
getJWT :: (MonadIO m) => Username -> Password -> KeycloakT m JWT
getJWT username password = do
  debug "Get user token"
  client <- viewConfig $ confAdapterConfig . confResource
  secret <- viewConfig $ confAdapterConfig . confCredentials . confSecret
  let dat =
        [ "client_id" := client,
          "client_secret" := secret,
          "grant_type" := ("password" :: Text),
          "password" := password,
          "username" := username
        ]
  body <- keycloakPost' "protocol/openid-connect/token" dat
  debug $ "Keycloak: " <> show body
  case eitherDecode body of
    Right ret -> do
      debug $ "Keycloak success: " <> show ret
      KeycloakT $ decodeCompact $ convertString $ accessToken ret
    Left err2 -> do
      debug $ "Keycloak parse error: " <> show err2
      kcError $ ParseError $ pack (show err2)

-- | return a Client token (linked to a Client, not a User). It is useful to create Resources in that Client in Keycloak.
getClientJWT :: (MonadIO m) => KeycloakT m JWT
getClientJWT = do
  debug "Get client token"
  client <- viewConfig $ confAdapterConfig . confResource
  secret <- viewConfig $ confAdapterConfig . confCredentials . confSecret
  let dat =
        [ "client_id" := client,
          "client_secret" := secret,
          "grant_type" := ("client_credentials" :: Text)
        ]
  body <- keycloakPost' "protocol/openid-connect/token" dat
  case eitherDecode body of
    Right ret -> do
      debug $ "Keycloak success: " <> show ret
      KeycloakT $ decodeCompact $ convertString $ accessToken ret
    Left err2 -> do
      debug $ "Keycloak parse error: " <> show err2
      kcError $ ParseError $ pack (show err2)

-- | Verify a JWT. If sucessful, the claims are returned. Otherwise, a JWTError is thrown.
verifyJWT :: forall m. (MonadTime m, MonadIO m) => JWT -> KeycloakT m KeycloakClaims
verifyJWT jwt = do
  jwks <- viewConfig confJWKs
  case jwks of
    [] -> kcError $ ParseError "No JWKs available for JWT verification"
    keys -> KeycloakT $ do
      -- Verify JWT and get standard claims
      -- Use JWKSet so jose library can try each key until one works
      stdClaims :: ClaimsSet <- verifyClaims (defaultJWTValidationSettings (const True)) (JWKSet keys) jwt
      -- Re-parse the ClaimsSet as JSON to get KeycloakClaims
      case fromJSON (toJSON stdClaims) of
        Success (keycloakClaims :: KeycloakClaims) -> return keycloakClaims
        Error err -> throwError $ ParseError $ pack $ "Failed to parse Keycloak claims: " <> err

-- | Extract the user identity from a token. Additional attributes can be encoded in the token.
getClaimsUser :: KeycloakClaims -> User
getClaimsUser kc =
  User
    { userId = Just $ UserId $ view (claimSub . _Just . string) (jwtClaims kc),
      userUsername = fromMaybe "" $ preferredUsername kc,
      userFirstName = givenName kc,
      userLastName = familyName kc,
      userEmail = emailClaim kc,
      userAttributes = additionalClaims kc
    }

-- | return JWKs from Keycloak. Its a set of keys that can be used to check signed tokens (JWTs)
-- This is done for you in the 'configureKeycloak' function. JWKs are stored in the Keycloak State Monad.
getJWKs :: Realm -> ServerURL -> IO [JWK]
getJWKs realm baseUrl = do
  let opts = W.defaults
  let url = unpack (baseUrl <> "/realms/" <> realm <> "/protocol/openid-connect/certs")
  info $ "Issuing KEYCLOAK GET with url: " <> show url
  debug $ "  headers: " <> show (opts ^. W.headers)
  res <- W.getWith opts url
  let body = res ^?! responseBody
  info $ show body
  case eitherDecode body of
    Right (JWKSet jwks) -> return jwks
    Left (err2 :: String) -> do
      debug $ "Keycloak parse error: " <> show err2
      error $ show err2
