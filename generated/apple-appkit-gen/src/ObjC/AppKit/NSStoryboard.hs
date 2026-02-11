{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStoryboard@.
module ObjC.AppKit.NSStoryboard
  ( NSStoryboard
  , IsNSStoryboard(..)
  , storyboardWithName_bundle
  , instantiateInitialController
  , instantiateInitialControllerWithCreator
  , instantiateControllerWithIdentifier
  , instantiateControllerWithIdentifier_creator
  , mainStoryboard
  , storyboardWithName_bundleSelector
  , instantiateInitialControllerSelector
  , instantiateInitialControllerWithCreatorSelector
  , instantiateControllerWithIdentifierSelector
  , instantiateControllerWithIdentifier_creatorSelector
  , mainStoryboardSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ storyboardWithName:bundle:@
storyboardWithName_bundle :: (IsNSString name, IsNSBundle storyboardBundleOrNil) => name -> storyboardBundleOrNil -> IO (Id NSStoryboard)
storyboardWithName_bundle name storyboardBundleOrNil =
  do
    cls' <- getRequiredClass "NSStoryboard"
    withObjCPtr name $ \raw_name ->
      withObjCPtr storyboardBundleOrNil $ \raw_storyboardBundleOrNil ->
        sendClassMsg cls' (mkSelector "storyboardWithName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_storyboardBundleOrNil :: Ptr ())] >>= retainedObject . castPtr

-- | @- instantiateInitialController@
instantiateInitialController :: IsNSStoryboard nsStoryboard => nsStoryboard -> IO RawId
instantiateInitialController nsStoryboard  =
    fmap (RawId . castPtr) $ sendMsg nsStoryboard (mkSelector "instantiateInitialController") (retPtr retVoid) []

-- | @- instantiateInitialControllerWithCreator:@
instantiateInitialControllerWithCreator :: IsNSStoryboard nsStoryboard => nsStoryboard -> RawId -> IO RawId
instantiateInitialControllerWithCreator nsStoryboard  block =
    fmap (RawId . castPtr) $ sendMsg nsStoryboard (mkSelector "instantiateInitialControllerWithCreator:") (retPtr retVoid) [argPtr (castPtr (unRawId block) :: Ptr ())]

-- | @- instantiateControllerWithIdentifier:@
instantiateControllerWithIdentifier :: (IsNSStoryboard nsStoryboard, IsNSString identifier) => nsStoryboard -> identifier -> IO RawId
instantiateControllerWithIdentifier nsStoryboard  identifier =
  withObjCPtr identifier $ \raw_identifier ->
      fmap (RawId . castPtr) $ sendMsg nsStoryboard (mkSelector "instantiateControllerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- instantiateControllerWithIdentifier:creator:@
instantiateControllerWithIdentifier_creator :: (IsNSStoryboard nsStoryboard, IsNSString identifier) => nsStoryboard -> identifier -> RawId -> IO RawId
instantiateControllerWithIdentifier_creator nsStoryboard  identifier block =
  withObjCPtr identifier $ \raw_identifier ->
      fmap (RawId . castPtr) $ sendMsg nsStoryboard (mkSelector "instantiateControllerWithIdentifier:creator:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr (unRawId block) :: Ptr ())]

-- | @+ mainStoryboard@
mainStoryboard :: IO (Id NSStoryboard)
mainStoryboard  =
  do
    cls' <- getRequiredClass "NSStoryboard"
    sendClassMsg cls' (mkSelector "mainStoryboard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @storyboardWithName:bundle:@
storyboardWithName_bundleSelector :: Selector
storyboardWithName_bundleSelector = mkSelector "storyboardWithName:bundle:"

-- | @Selector@ for @instantiateInitialController@
instantiateInitialControllerSelector :: Selector
instantiateInitialControllerSelector = mkSelector "instantiateInitialController"

-- | @Selector@ for @instantiateInitialControllerWithCreator:@
instantiateInitialControllerWithCreatorSelector :: Selector
instantiateInitialControllerWithCreatorSelector = mkSelector "instantiateInitialControllerWithCreator:"

-- | @Selector@ for @instantiateControllerWithIdentifier:@
instantiateControllerWithIdentifierSelector :: Selector
instantiateControllerWithIdentifierSelector = mkSelector "instantiateControllerWithIdentifier:"

-- | @Selector@ for @instantiateControllerWithIdentifier:creator:@
instantiateControllerWithIdentifier_creatorSelector :: Selector
instantiateControllerWithIdentifier_creatorSelector = mkSelector "instantiateControllerWithIdentifier:creator:"

-- | @Selector@ for @mainStoryboard@
mainStoryboardSelector :: Selector
mainStoryboardSelector = mkSelector "mainStoryboard"

