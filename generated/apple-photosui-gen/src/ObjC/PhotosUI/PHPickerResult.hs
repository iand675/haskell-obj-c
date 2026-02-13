{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A user selected asset from @PHPickerViewController.@
--
-- Generated bindings for @PHPickerResult@.
module ObjC.PhotosUI.PHPickerResult
  ( PHPickerResult
  , IsPHPickerResult(..)
  , new
  , init_
  , itemProvider
  , assetIdentifier
  , assetIdentifierSelector
  , initSelector
  , itemProviderSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPickerResult)
new  =
  do
    cls' <- getRequiredClass "PHPickerResult"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPHPickerResult phPickerResult => phPickerResult -> IO (Id PHPickerResult)
init_ phPickerResult =
  sendOwnedMessage phPickerResult initSelector

-- | Representations of the selected asset.
--
-- ObjC selector: @- itemProvider@
itemProvider :: IsPHPickerResult phPickerResult => phPickerResult -> IO (Id NSItemProvider)
itemProvider phPickerResult =
  sendMessage phPickerResult itemProviderSelector

-- | The local identifier of the selected asset.
--
-- ObjC selector: @- assetIdentifier@
assetIdentifier :: IsPHPickerResult phPickerResult => phPickerResult -> IO (Id NSString)
assetIdentifier phPickerResult =
  sendMessage phPickerResult assetIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHPickerResult)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHPickerResult)
initSelector = mkSelector "init"

-- | @Selector@ for @itemProvider@
itemProviderSelector :: Selector '[] (Id NSItemProvider)
itemProviderSelector = mkSelector "itemProvider"

-- | @Selector@ for @assetIdentifier@
assetIdentifierSelector :: Selector '[] (Id NSString)
assetIdentifierSelector = mkSelector "assetIdentifier"

