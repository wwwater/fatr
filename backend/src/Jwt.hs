{-#LANGUAGE OverloadedStrings #-}

module Jwt where

import Web.JWT                  (iat,
                                exp,
                                unregisteredClaims,
                                secret,
                                encodeSigned,
                                decodeAndVerifySignature,
                                Algorithm(HS256),
                                def,
                                JSON,
                                numericDate,
                                claims,
                                secondsSinceEpoch)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Data.Text                (pack)
import Data.Map as Map          (fromList, lookup)
import Data.Aeson.Types         (Value(String), parseMaybe, parseJSON)

import Model                    as M



createJwt :: M.JwtSecret -> String -> IO JSON
createJwt key user = do
  now <- getPOSIXTime
  let expire = now + 60 * 60 * 24 -- 24 hours
      claimsSet = def {
                      iat = numericDate now,
                      Web.JWT.exp = numericDate expire,
                      unregisteredClaims = fromList [("user", String (pack user))]
                      }
   in return $ encodeSigned HS256 (secret (pack key)) claimsSet

getUserIfValidJwt :: M.JwtSecret -> M.JwtToken -> IO (Maybe String)
getUserIfValidJwt key jwt =
  case decodeAndVerifySignature (secret (pack key)) (pack jwt) of
    Just verifiedJwt ->
      case Web.JWT.exp $ claims verifiedJwt of
        Just expiredTime -> do
          now <- getPOSIXTime
          let expire = secondsSinceEpoch expiredTime
            in return $
              if now < expire
                then
                  let claimsMap = unregisteredClaims (claims verifiedJwt)
                  in Map.lookup (pack "user") claimsMap >>= parseMaybe parseJSON
                else Nothing
        Nothing -> return Nothing
    Nothing -> return Nothing
