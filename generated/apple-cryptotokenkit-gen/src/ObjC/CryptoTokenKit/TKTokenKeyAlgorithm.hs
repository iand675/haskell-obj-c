{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKTokenKeyAlgorithm Encapsulates cryptographic algorithm, possibly with additional associated required algorithms.
--
-- An algorithm supported by a key can be usually described by one value of @SecKeyAlgorithm@ enumeration.  However, some tokens (notably smartcards) require that input data for the operation are in generic format, but that generic format must be formatted according to some more specific algorithm.  An example for this would be token accepting raw data for cryptographic signature but requiring that raw data are formatted according to PKCS1 padding rules.  To express such requirement, TKTokenKeyAlgorithm defines target algorithm (@kSecKeyAlgorithmRSASignatureRaw@ in our example) and a set of other algorithms which were used (continuing example above, @kSecKeyAlgorithmRSASignatureDigestPKCS1v15SHA1@ will be reported as supported).
--
-- Generated bindings for @TKTokenKeyAlgorithm@.
module ObjC.CryptoTokenKit.TKTokenKeyAlgorithm
  ( TKTokenKeyAlgorithm
  , IsTKTokenKeyAlgorithm(..)
  , init_
  , isAlgorithm
  , supportsAlgorithm
  , initSelector
  , isAlgorithmSelector
  , supportsAlgorithmSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsTKTokenKeyAlgorithm tkTokenKeyAlgorithm => tkTokenKeyAlgorithm -> IO (Id TKTokenKeyAlgorithm)
init_ tkTokenKeyAlgorithm =
  sendOwnedMessage tkTokenKeyAlgorithm initSelector

-- | Checks if specified algorithm is base operation algorithm.
--
-- ObjC selector: @- isAlgorithm:@
isAlgorithm :: IsTKTokenKeyAlgorithm tkTokenKeyAlgorithm => tkTokenKeyAlgorithm -> RawId -> IO Bool
isAlgorithm tkTokenKeyAlgorithm algorithm =
  sendMessage tkTokenKeyAlgorithm isAlgorithmSelector algorithm

-- | Checks whether specified algorithm is either target algorithm or one of the algorithms through which the operation passed.
--
-- ObjC selector: @- supportsAlgorithm:@
supportsAlgorithm :: IsTKTokenKeyAlgorithm tkTokenKeyAlgorithm => tkTokenKeyAlgorithm -> RawId -> IO Bool
supportsAlgorithm tkTokenKeyAlgorithm algorithm =
  sendMessage tkTokenKeyAlgorithm supportsAlgorithmSelector algorithm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenKeyAlgorithm)
initSelector = mkSelector "init"

-- | @Selector@ for @isAlgorithm:@
isAlgorithmSelector :: Selector '[RawId] Bool
isAlgorithmSelector = mkSelector "isAlgorithm:"

-- | @Selector@ for @supportsAlgorithm:@
supportsAlgorithmSelector :: Selector '[RawId] Bool
supportsAlgorithmSelector = mkSelector "supportsAlgorithm:"

