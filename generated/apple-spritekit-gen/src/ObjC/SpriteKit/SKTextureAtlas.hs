{-# LANGUAGE DataKinds #-}
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
  , preloadTextureAtlases_withCompletionHandlerSelector
  , preloadWithCompletionHandlerSelector
  , textureNamedSelector
  , textureNamesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ atlasNamed:@
atlasNamed :: IsNSString name => name -> IO (Id SKTextureAtlas)
atlasNamed name =
  do
    cls' <- getRequiredClass "SKTextureAtlas"
    sendClassMessage cls' atlasNamedSelector (toNSString name)

-- | @+ atlasWithDictionary:@
atlasWithDictionary :: IsNSDictionary properties => properties -> IO (Id SKTextureAtlas)
atlasWithDictionary properties =
  do
    cls' <- getRequiredClass "SKTextureAtlas"
    sendClassMessage cls' atlasWithDictionarySelector (toNSDictionary properties)

-- | @- textureNamed:@
textureNamed :: (IsSKTextureAtlas skTextureAtlas, IsNSString name) => skTextureAtlas -> name -> IO (Id SKTexture)
textureNamed skTextureAtlas name =
  sendMessage skTextureAtlas textureNamedSelector (toNSString name)

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
    sendClassMessage cls' preloadTextureAtlases_withCompletionHandlerSelector (toNSArray textureAtlases) completionHandler

-- | Request that this texture atlas be loaded into vram on the next render update, with a callback handler.
--
-- ObjC selector: @- preloadWithCompletionHandler:@
preloadWithCompletionHandler :: IsSKTextureAtlas skTextureAtlas => skTextureAtlas -> Ptr () -> IO ()
preloadWithCompletionHandler skTextureAtlas completionHandler =
  sendMessage skTextureAtlas preloadWithCompletionHandlerSelector completionHandler

-- | @- textureNames@
textureNames :: IsSKTextureAtlas skTextureAtlas => skTextureAtlas -> IO (Id NSArray)
textureNames skTextureAtlas =
  sendMessage skTextureAtlas textureNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @atlasNamed:@
atlasNamedSelector :: Selector '[Id NSString] (Id SKTextureAtlas)
atlasNamedSelector = mkSelector "atlasNamed:"

-- | @Selector@ for @atlasWithDictionary:@
atlasWithDictionarySelector :: Selector '[Id NSDictionary] (Id SKTextureAtlas)
atlasWithDictionarySelector = mkSelector "atlasWithDictionary:"

-- | @Selector@ for @textureNamed:@
textureNamedSelector :: Selector '[Id NSString] (Id SKTexture)
textureNamedSelector = mkSelector "textureNamed:"

-- | @Selector@ for @preloadTextureAtlases:withCompletionHandler:@
preloadTextureAtlases_withCompletionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
preloadTextureAtlases_withCompletionHandlerSelector = mkSelector "preloadTextureAtlases:withCompletionHandler:"

-- | @Selector@ for @preloadWithCompletionHandler:@
preloadWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
preloadWithCompletionHandlerSelector = mkSelector "preloadWithCompletionHandler:"

-- | @Selector@ for @textureNames@
textureNamesSelector :: Selector '[] (Id NSArray)
textureNamesSelector = mkSelector "textureNames"

