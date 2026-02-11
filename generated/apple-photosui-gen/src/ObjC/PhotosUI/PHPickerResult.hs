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
  , newSelector
  , initSelector
  , itemProviderSelector
  , assetIdentifierSelector


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

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPickerResult)
new  =
  do
    cls' <- getRequiredClass "PHPickerResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHPickerResult phPickerResult => phPickerResult -> IO (Id PHPickerResult)
init_ phPickerResult  =
    sendMsg phPickerResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Representations of the selected asset.
--
-- ObjC selector: @- itemProvider@
itemProvider :: IsPHPickerResult phPickerResult => phPickerResult -> IO (Id NSItemProvider)
itemProvider phPickerResult  =
    sendMsg phPickerResult (mkSelector "itemProvider") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The local identifier of the selected asset.
--
-- ObjC selector: @- assetIdentifier@
assetIdentifier :: IsPHPickerResult phPickerResult => phPickerResult -> IO (Id NSString)
assetIdentifier phPickerResult  =
    sendMsg phPickerResult (mkSelector "assetIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @itemProvider@
itemProviderSelector :: Selector
itemProviderSelector = mkSelector "itemProvider"

-- | @Selector@ for @assetIdentifier@
assetIdentifierSelector :: Selector
assetIdentifierSelector = mkSelector "assetIdentifier"

