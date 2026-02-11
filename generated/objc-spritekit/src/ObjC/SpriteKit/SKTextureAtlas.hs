{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKTextureAtlas@.
module ObjC.SpriteKit.SKTextureAtlas
  ( SKTextureAtlas
  , IsSKTextureAtlas(..)
  , atlasNamed
  , atlasWithDictionary
  , textureNamed
  , preloadTextureAtlases_withCompletionHandler
  , preloadWithCompletionHandler
  , textureNames
  , atlasNamedSelector
  , atlasWithDictionarySelector
  , textureNamedSelector
  , preloadTextureAtlases_withCompletionHandlerSelector
  , preloadWithCompletionHandlerSelector
  , textureNamesSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ atlasNamed:@
atlasNamed :: IsNSString name => name -> IO (Id SKTextureAtlas)
atlasNamed name =
  do
    cls' <- getRequiredClass "SKTextureAtlas"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "atlasNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ atlasWithDictionary:@
atlasWithDictionary :: IsNSDictionary properties => properties -> IO (Id SKTextureAtlas)
atlasWithDictionary properties =
  do
    cls' <- getRequiredClass "SKTextureAtlas"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "atlasWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_properties :: Ptr ())] >>= retainedObject . castPtr

-- | @- textureNamed:@
textureNamed :: (IsSKTextureAtlas skTextureAtlas, IsNSString name) => skTextureAtlas -> name -> IO (Id SKTexture)
textureNamed skTextureAtlas  name =
withObjCPtr name $ \raw_name ->
    sendMsg skTextureAtlas (mkSelector "textureNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | Start a texture atlas preload operation on an array of texture atlas
--
-- @textureAtlases@ — an array of SKTextureAtlas to be preloaded
--
-- @completionHandler@ — will be called upon the preload completion
--
-- ObjC selector: @+ preloadTextureAtlases:withCompletionHandler:@
preloadTextureAtlases_withCompletionHandler :: IsNSArray textureAtlases => textureAtlases -> Ptr () -> IO ()
preloadTextureAtlases_withCompletionHandler textureAtlases completionHandler =
  do
    cls' <- getRequiredClass "SKTextureAtlas"
    withObjCPtr textureAtlases $ \raw_textureAtlases ->
      sendClassMsg cls' (mkSelector "preloadTextureAtlases:withCompletionHandler:") retVoid [argPtr (castPtr raw_textureAtlases :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Request that this texture atlas be loaded into vram on the next render update, with a callback handler.
--
-- ObjC selector: @- preloadWithCompletionHandler:@
preloadWithCompletionHandler :: IsSKTextureAtlas skTextureAtlas => skTextureAtlas -> Ptr () -> IO ()
preloadWithCompletionHandler skTextureAtlas  completionHandler =
  sendMsg skTextureAtlas (mkSelector "preloadWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- textureNames@
textureNames :: IsSKTextureAtlas skTextureAtlas => skTextureAtlas -> IO (Id NSArray)
textureNames skTextureAtlas  =
  sendMsg skTextureAtlas (mkSelector "textureNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @atlasNamed:@
atlasNamedSelector :: Selector
atlasNamedSelector = mkSelector "atlasNamed:"

-- | @Selector@ for @atlasWithDictionary:@
atlasWithDictionarySelector :: Selector
atlasWithDictionarySelector = mkSelector "atlasWithDictionary:"

-- | @Selector@ for @textureNamed:@
textureNamedSelector :: Selector
textureNamedSelector = mkSelector "textureNamed:"

-- | @Selector@ for @preloadTextureAtlases:withCompletionHandler:@
preloadTextureAtlases_withCompletionHandlerSelector :: Selector
preloadTextureAtlases_withCompletionHandlerSelector = mkSelector "preloadTextureAtlases:withCompletionHandler:"

-- | @Selector@ for @preloadWithCompletionHandler:@
preloadWithCompletionHandlerSelector :: Selector
preloadWithCompletionHandlerSelector = mkSelector "preloadWithCompletionHandler:"

-- | @Selector@ for @textureNames@
textureNamesSelector :: Selector
textureNamesSelector = mkSelector "textureNames"

