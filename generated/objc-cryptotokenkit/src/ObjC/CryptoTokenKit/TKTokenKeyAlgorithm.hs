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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsTKTokenKeyAlgorithm tkTokenKeyAlgorithm => tkTokenKeyAlgorithm -> IO (Id TKTokenKeyAlgorithm)
init_ tkTokenKeyAlgorithm  =
  sendMsg tkTokenKeyAlgorithm (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Checks if specified algorithm is base operation algorithm.
--
-- ObjC selector: @- isAlgorithm:@
isAlgorithm :: IsTKTokenKeyAlgorithm tkTokenKeyAlgorithm => tkTokenKeyAlgorithm -> RawId -> IO Bool
isAlgorithm tkTokenKeyAlgorithm  algorithm =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkTokenKeyAlgorithm (mkSelector "isAlgorithm:") retCULong [argPtr (castPtr (unRawId algorithm) :: Ptr ())]

-- | Checks whether specified algorithm is either target algorithm or one of the algorithms through which the operation passed.
--
-- ObjC selector: @- supportsAlgorithm:@
supportsAlgorithm :: IsTKTokenKeyAlgorithm tkTokenKeyAlgorithm => tkTokenKeyAlgorithm -> RawId -> IO Bool
supportsAlgorithm tkTokenKeyAlgorithm  algorithm =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkTokenKeyAlgorithm (mkSelector "supportsAlgorithm:") retCULong [argPtr (castPtr (unRawId algorithm) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @isAlgorithm:@
isAlgorithmSelector :: Selector
isAlgorithmSelector = mkSelector "isAlgorithm:"

-- | @Selector@ for @supportsAlgorithm:@
supportsAlgorithmSelector :: Selector
supportsAlgorithmSelector = mkSelector "supportsAlgorithm:"

