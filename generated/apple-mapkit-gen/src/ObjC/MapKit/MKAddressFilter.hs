{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , excludesOptionsSelector
  , filterExcludingAllSelector
  , filterIncludingAllSelector
  , includesOptionsSelector
  , initExcludingOptionsSelector
  , initIncludingOptionsSelector

  -- * Enum types
  , MKAddressFilterOption(MKAddressFilterOption)
  , pattern MKAddressFilterOptionCountry
  , pattern MKAddressFilterOptionAdministrativeArea
  , pattern MKAddressFilterOptionSubAdministrativeArea
  , pattern MKAddressFilterOptionLocality
  , pattern MKAddressFilterOptionSubLocality
  , pattern MKAddressFilterOptionPostalCode

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initIncludingOptions:@
initIncludingOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO (Id MKAddressFilter)
initIncludingOptions mkAddressFilter options =
  sendOwnedMessage mkAddressFilter initIncludingOptionsSelector options

-- | @- initExcludingOptions:@
initExcludingOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO (Id MKAddressFilter)
initExcludingOptions mkAddressFilter options =
  sendOwnedMessage mkAddressFilter initExcludingOptionsSelector options

-- | @- includesOptions:@
includesOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO Bool
includesOptions mkAddressFilter options =
  sendMessage mkAddressFilter includesOptionsSelector options

-- | @- excludesOptions:@
excludesOptions :: IsMKAddressFilter mkAddressFilter => mkAddressFilter -> MKAddressFilterOption -> IO Bool
excludesOptions mkAddressFilter options =
  sendMessage mkAddressFilter excludesOptionsSelector options

-- | @+ filterIncludingAll@
filterIncludingAll :: IO (Id MKAddressFilter)
filterIncludingAll  =
  do
    cls' <- getRequiredClass "MKAddressFilter"
    sendClassMessage cls' filterIncludingAllSelector

-- | @+ filterExcludingAll@
filterExcludingAll :: IO (Id MKAddressFilter)
filterExcludingAll  =
  do
    cls' <- getRequiredClass "MKAddressFilter"
    sendClassMessage cls' filterExcludingAllSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initIncludingOptions:@
initIncludingOptionsSelector :: Selector '[MKAddressFilterOption] (Id MKAddressFilter)
initIncludingOptionsSelector = mkSelector "initIncludingOptions:"

-- | @Selector@ for @initExcludingOptions:@
initExcludingOptionsSelector :: Selector '[MKAddressFilterOption] (Id MKAddressFilter)
initExcludingOptionsSelector = mkSelector "initExcludingOptions:"

-- | @Selector@ for @includesOptions:@
includesOptionsSelector :: Selector '[MKAddressFilterOption] Bool
includesOptionsSelector = mkSelector "includesOptions:"

-- | @Selector@ for @excludesOptions:@
excludesOptionsSelector :: Selector '[MKAddressFilterOption] Bool
excludesOptionsSelector = mkSelector "excludesOptions:"

-- | @Selector@ for @filterIncludingAll@
filterIncludingAllSelector :: Selector '[] (Id MKAddressFilter)
filterIncludingAllSelector = mkSelector "filterIncludingAll"

-- | @Selector@ for @filterExcludingAll@
filterExcludingAllSelector :: Selector '[] (Id MKAddressFilter)
filterExcludingAllSelector = mkSelector "filterExcludingAll"

