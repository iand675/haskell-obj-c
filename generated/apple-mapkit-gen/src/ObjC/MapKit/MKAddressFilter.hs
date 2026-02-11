{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKAddressFilter@.
module ObjC.MapKit.MKAddressFilter
  ( MKAddressFilter
  , IsMKAddressFilter(..)
  , initIncludingOptions
  , initExcludingOptions
  , includesOptions
  , excludesOptions
  , filterIncludingAll
  , filterExcludingAll
  , initIncludingOptionsSelector
  , initExcludingOptionsSelector
  , includesOptionsSelector
  , excludesOptionsSelector
  , filterIncludingAllSelector
  , filterExcludingAllSelector

  -- * Enum types
  , MKAddressFilterOption(MKAddressFilterOption)
  , pattern MKAddressFilterOptionCountry
  , pattern MKAddressFilterOptionAdministrativeArea
  , pattern MKAddressFilterOptionSubAdministrativeArea
  , pattern MKAddressFilterOptionLocality
  , pattern MKAddressFilterOptionSubLocality
  , pattern MKAddressFilterOptionPostalCode

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

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initIncludingOptions:@
initIncludingOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO (Id MKAddressFilter)
initIncludingOptions mkAddressFilter  options =
    sendMsg mkAddressFilter (mkSelector "initIncludingOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- initExcludingOptions:@
initExcludingOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO (Id MKAddressFilter)
initExcludingOptions mkAddressFilter  options =
    sendMsg mkAddressFilter (mkSelector "initExcludingOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- includesOptions:@
includesOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO Bool
includesOptions mkAddressFilter  options =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkAddressFilter (mkSelector "includesOptions:") retCULong [argCULong (coerce options)]

-- | @- excludesOptions:@
excludesOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO Bool
excludesOptions mkAddressFilter  options =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkAddressFilter (mkSelector "excludesOptions:") retCULong [argCULong (coerce options)]

-- | @+ filterIncludingAll@
filterIncludingAll :: IO (Id MKAddressFilter)
filterIncludingAll  =
  do
    cls' <- getRequiredClass "MKAddressFilter"
    sendClassMsg cls' (mkSelector "filterIncludingAll") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ filterExcludingAll@
filterExcludingAll :: IO (Id MKAddressFilter)
filterExcludingAll  =
  do
    cls' <- getRequiredClass "MKAddressFilter"
    sendClassMsg cls' (mkSelector "filterExcludingAll") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initIncludingOptions:@
initIncludingOptionsSelector :: Selector
initIncludingOptionsSelector = mkSelector "initIncludingOptions:"

-- | @Selector@ for @initExcludingOptions:@
initExcludingOptionsSelector :: Selector
initExcludingOptionsSelector = mkSelector "initExcludingOptions:"

-- | @Selector@ for @includesOptions:@
includesOptionsSelector :: Selector
includesOptionsSelector = mkSelector "includesOptions:"

-- | @Selector@ for @excludesOptions:@
excludesOptionsSelector :: Selector
excludesOptionsSelector = mkSelector "excludesOptions:"

-- | @Selector@ for @filterIncludingAll@
filterIncludingAllSelector :: Selector
filterIncludingAllSelector = mkSelector "filterIncludingAll"

-- | @Selector@ for @filterExcludingAll@
filterExcludingAllSelector :: Selector
filterExcludingAllSelector = mkSelector "filterExcludingAll"

