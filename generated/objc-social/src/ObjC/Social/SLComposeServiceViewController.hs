{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SLComposeServiceViewController@.
module ObjC.Social.SLComposeServiceViewController
  ( SLComposeServiceViewController
  , IsSLComposeServiceViewController(..)
  , presentationAnimationDidFinish
  , didSelectPost
  , didSelectCancel
  , cancel
  , isContentValid
  , validateContent
  , textView
  , contentText
  , placeholder
  , setPlaceholder
  , charactersRemaining
  , setCharactersRemaining
  , presentationAnimationDidFinishSelector
  , didSelectPostSelector
  , didSelectCancelSelector
  , cancelSelector
  , isContentValidSelector
  , validateContentSelector
  , textViewSelector
  , contentTextSelector
  , placeholderSelector
  , setPlaceholderSelector
  , charactersRemainingSelector
  , setCharactersRemainingSelector


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

import ObjC.Social.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentationAnimationDidFinish@
presentationAnimationDidFinish :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
presentationAnimationDidFinish slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "presentationAnimationDidFinish") retVoid []

-- | @- didSelectPost@
didSelectPost :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
didSelectPost slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "didSelectPost") retVoid []

-- | @- didSelectCancel@
didSelectCancel :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
didSelectCancel slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "didSelectCancel") retVoid []

-- | @- cancel@
cancel :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
cancel slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "cancel") retVoid []

-- | @- isContentValid@
isContentValid :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO Bool
isContentValid slComposeServiceViewController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg slComposeServiceViewController (mkSelector "isContentValid") retCULong []

-- | @- validateContent@
validateContent :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
validateContent slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "validateContent") retVoid []

-- | @- textView@
textView :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSTextView)
textView slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "textView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentText@
contentText :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSString)
contentText slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "contentText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- placeholder@
placeholder :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSString)
placeholder slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "placeholder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaceholder:@
setPlaceholder :: (IsSLComposeServiceViewController slComposeServiceViewController, IsNSString value) => slComposeServiceViewController -> value -> IO ()
setPlaceholder slComposeServiceViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg slComposeServiceViewController (mkSelector "setPlaceholder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- charactersRemaining@
charactersRemaining :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSNumber)
charactersRemaining slComposeServiceViewController  =
  sendMsg slComposeServiceViewController (mkSelector "charactersRemaining") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharactersRemaining:@
setCharactersRemaining :: (IsSLComposeServiceViewController slComposeServiceViewController, IsNSNumber value) => slComposeServiceViewController -> value -> IO ()
setCharactersRemaining slComposeServiceViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg slComposeServiceViewController (mkSelector "setCharactersRemaining:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentationAnimationDidFinish@
presentationAnimationDidFinishSelector :: Selector
presentationAnimationDidFinishSelector = mkSelector "presentationAnimationDidFinish"

-- | @Selector@ for @didSelectPost@
didSelectPostSelector :: Selector
didSelectPostSelector = mkSelector "didSelectPost"

-- | @Selector@ for @didSelectCancel@
didSelectCancelSelector :: Selector
didSelectCancelSelector = mkSelector "didSelectCancel"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @isContentValid@
isContentValidSelector :: Selector
isContentValidSelector = mkSelector "isContentValid"

-- | @Selector@ for @validateContent@
validateContentSelector :: Selector
validateContentSelector = mkSelector "validateContent"

-- | @Selector@ for @textView@
textViewSelector :: Selector
textViewSelector = mkSelector "textView"

-- | @Selector@ for @contentText@
contentTextSelector :: Selector
contentTextSelector = mkSelector "contentText"

-- | @Selector@ for @placeholder@
placeholderSelector :: Selector
placeholderSelector = mkSelector "placeholder"

-- | @Selector@ for @setPlaceholder:@
setPlaceholderSelector :: Selector
setPlaceholderSelector = mkSelector "setPlaceholder:"

-- | @Selector@ for @charactersRemaining@
charactersRemainingSelector :: Selector
charactersRemainingSelector = mkSelector "charactersRemaining"

-- | @Selector@ for @setCharactersRemaining:@
setCharactersRemainingSelector :: Selector
setCharactersRemainingSelector = mkSelector "setCharactersRemaining:"

