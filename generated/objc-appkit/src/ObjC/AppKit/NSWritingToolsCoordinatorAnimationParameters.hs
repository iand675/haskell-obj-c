{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object you use to configure additional tasks or animations to run alongside the Writing Tools animations.
--
-- When Writing Tools replaces text in one of your context objects, it provides an @NSWritingToolsCoordinator.AnimationParameters@ object for you to use to configure any additional animations. During a Writing Tools session, you hide the text under evaluation and provide a targeted preview of your content. Writing Tools animations changes to that preview, but you might need to provide additional animations for other parts of your view’s content. For example, you might need to animate any layout changes caused by the insertion or removal of text in other parts of your view. Use this object to configure those animations.
--
-- You don’t create an @NSWritingToolsCoordinator.AnimationParameters@ object directly. Instead, the system creates one and passes it to the``NSWritingToolsCoordinator/writingToolsCoordinator(_:replaceRange:inContext:proposedText:reason:animationParameters:completion:)``method of your ``NSWritingToolsCoordinator/Delegate`` object. Use thatobject to specify the blocks to run during and after the system animations.
--
-- Generated bindings for @NSWritingToolsCoordinatorAnimationParameters@.
module ObjC.AppKit.NSWritingToolsCoordinatorAnimationParameters
  ( NSWritingToolsCoordinatorAnimationParameters
  , IsNSWritingToolsCoordinatorAnimationParameters(..)
  , init_
  , duration
  , delay
  , progressHandler
  , setProgressHandler
  , completionHandler
  , setCompletionHandler
  , initSelector
  , durationSelector
  , delaySelector
  , progressHandlerSelector
  , setProgressHandlerSelector
  , completionHandlerSelector
  , setCompletionHandlerSelector


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

-- | @- init@
init_ :: IsNSWritingToolsCoordinatorAnimationParameters nsWritingToolsCoordinatorAnimationParameters => nsWritingToolsCoordinatorAnimationParameters -> IO (Id NSWritingToolsCoordinatorAnimationParameters)
init_ nsWritingToolsCoordinatorAnimationParameters  =
  sendMsg nsWritingToolsCoordinatorAnimationParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The number of seconds it takes the system animations to run.
--
-- ObjC selector: @- duration@
duration :: IsNSWritingToolsCoordinatorAnimationParameters nsWritingToolsCoordinatorAnimationParameters => nsWritingToolsCoordinatorAnimationParameters -> IO CDouble
duration nsWritingToolsCoordinatorAnimationParameters  =
  sendMsg nsWritingToolsCoordinatorAnimationParameters (mkSelector "duration") retCDouble []

-- | The number of seconds the system waits before starting its animations.
--
-- ObjC selector: @- delay@
delay :: IsNSWritingToolsCoordinatorAnimationParameters nsWritingToolsCoordinatorAnimationParameters => nsWritingToolsCoordinatorAnimationParameters -> IO CDouble
delay nsWritingToolsCoordinatorAnimationParameters  =
  sendMsg nsWritingToolsCoordinatorAnimationParameters (mkSelector "delay") retCDouble []

-- | A custom block that runs at the same time as the system animations.
--
-- If you have animations you want to run at the same time as the system animations, assign a block to this property and use it to run your animations. The block you provide must have no return value and take a floating-point value as a parameter. The parameter indicates the current progress of the animations as a percentage value between @0.0@ to @1.0@. The system executes your block multiple times during the course of the animations, providing an updated completion value each time.
--
-- ObjC selector: @- progressHandler@
progressHandler :: IsNSWritingToolsCoordinatorAnimationParameters nsWritingToolsCoordinatorAnimationParameters => nsWritingToolsCoordinatorAnimationParameters -> IO (Ptr ())
progressHandler nsWritingToolsCoordinatorAnimationParameters  =
  fmap castPtr $ sendMsg nsWritingToolsCoordinatorAnimationParameters (mkSelector "progressHandler") (retPtr retVoid) []

-- | A custom block that runs at the same time as the system animations.
--
-- If you have animations you want to run at the same time as the system animations, assign a block to this property and use it to run your animations. The block you provide must have no return value and take a floating-point value as a parameter. The parameter indicates the current progress of the animations as a percentage value between @0.0@ to @1.0@. The system executes your block multiple times during the course of the animations, providing an updated completion value each time.
--
-- ObjC selector: @- setProgressHandler:@
setProgressHandler :: IsNSWritingToolsCoordinatorAnimationParameters nsWritingToolsCoordinatorAnimationParameters => nsWritingToolsCoordinatorAnimationParameters -> Ptr () -> IO ()
setProgressHandler nsWritingToolsCoordinatorAnimationParameters  value =
  sendMsg nsWritingToolsCoordinatorAnimationParameters (mkSelector "setProgressHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | A custom block to run when the system animations finish.
--
-- Set this property to a block that you want the system to run when any animations finish. The block you provide must have no return value and no parameters. The system executes this block once when the current animation finish.
--
-- ObjC selector: @- completionHandler@
completionHandler :: IsNSWritingToolsCoordinatorAnimationParameters nsWritingToolsCoordinatorAnimationParameters => nsWritingToolsCoordinatorAnimationParameters -> IO (Ptr ())
completionHandler nsWritingToolsCoordinatorAnimationParameters  =
  fmap castPtr $ sendMsg nsWritingToolsCoordinatorAnimationParameters (mkSelector "completionHandler") (retPtr retVoid) []

-- | A custom block to run when the system animations finish.
--
-- Set this property to a block that you want the system to run when any animations finish. The block you provide must have no return value and no parameters. The system executes this block once when the current animation finish.
--
-- ObjC selector: @- setCompletionHandler:@
setCompletionHandler :: IsNSWritingToolsCoordinatorAnimationParameters nsWritingToolsCoordinatorAnimationParameters => nsWritingToolsCoordinatorAnimationParameters -> Ptr () -> IO ()
setCompletionHandler nsWritingToolsCoordinatorAnimationParameters  value =
  sendMsg nsWritingToolsCoordinatorAnimationParameters (mkSelector "setCompletionHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @delay@
delaySelector :: Selector
delaySelector = mkSelector "delay"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector
setProgressHandlerSelector = mkSelector "setProgressHandler:"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @setCompletionHandler:@
setCompletionHandlerSelector :: Selector
setCompletionHandlerSelector = mkSelector "setCompletionHandler:"

