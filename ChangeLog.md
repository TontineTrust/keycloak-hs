# Changelog for keycloak-hs

## 3.1.1.0 - 2025-10-25

### Changed
- Optimized `ClientCredentials` from `data` to `newtype` for better performance
- Added `deriving newtype` for `KeycloakT` with explicit typeclass instances (Monad, Applicative, Functor, MonadIO, MonadTime, MonadError)
- Added `MonadTrans` instance for `KeycloakT`

## 3.1.0.0 - 2025-10-24

### Added
- New `KeycloakClaims` type that wraps `ClaimsSet` with Keycloak-specific fields
- Support for parsing Keycloak-specific JWT claims (preferred_username, given_name, family_name, email)
- `HasClaimsSet` instance for `KeycloakClaims` for compatibility with jose library

### Changed
- **Breaking**: `verifyJWT` now returns `KeycloakClaims` instead of `ClaimsSet`
- **Breaking**: `getClaimsUser` now accepts `KeycloakClaims` instead of `ClaimsSet`
- Removed all usage of deprecated `unregisteredClaims` from jose library
- Updated dependency bounds for compatibility with modern GHC and libraries:
  - `bytestring`: < 0.13
  - `containers`: < 0.8
  - `hashable`: < 1.6
  - `http-api-data`: < 0.7
  - `jose`: < 0.12
  - `lens`: < 5.4
  - `mtl`: < 2.4
  - `text`: < 2.2

### Fixed
- Fixed ambiguous `show` import errors with newer Text library versions
- Fixed missing imports (`join`, `void`) in Authorizations module
- Fixed partial function usage (replaced `head` with safe pattern matching)
- Fixed name shadowing warning in example code

## Unreleased changes
