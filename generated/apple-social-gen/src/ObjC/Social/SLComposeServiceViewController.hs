{-# LANGUAGE DataKinds #-}
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
  , cancelSelector
  , charactersRemainingSelector
  , contentTextSelector
  , didSelectCancelSelector
  , didSelectPostSelector
  , isContentValidSelector
  , placeholderSelector
  , presentationAnimationDidFinishSelector
  , setCharactersRemainingSelector
  , setPlaceholderSelector
  , textViewSelector
  , validateContentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Social.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presentationAnimationDidFinish@
presentationAnimationDidFinish :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
presentationAnimationDidFinish slComposeServiceViewController =
  sendMessage slComposeServiceViewController presentationAnimationDidFinishSelector

-- | @- didSelectPost@
didSelectPost :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
didSelectPost slComposeServiceViewController =
  sendMessage slComposeServiceViewController didSelectPostSelector

-- | @- didSelectCancel@
didSelectCancel :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
didSelectCancel slComposeServiceViewController =
  sendMessage slComposeServiceViewController didSelectCancelSelector

-- | @- cancel@
cancel :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
cancel slComposeServiceViewController =
  sendMessage slComposeServiceViewController cancelSelector

-- | @- isContentValid@
isContentValid :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO Bool
isContentValid slComposeServiceViewController =
  sendMessage slComposeServiceViewController isContentValidSelector

-- | @- validateContent@
validateContent :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO ()
validateContent slComposeServiceViewController =
  sendMessage slComposeServiceViewController validateContentSelector

-- | @- textView@
textView :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSTextView)
textView slComposeServiceViewController =
  sendMessage slComposeServiceViewController textViewSelector

-- | @- contentText@
contentText :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSString)
contentText slComposeServiceViewController =
  sendMessage slComposeServiceViewController contentTextSelector

-- | @- placeholder@
placeholder :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSString)
placeholder slComposeServiceViewController =
  sendMessage slComposeServiceViewController placeholderSelector

-- | @- setPlaceholder:@
setPlaceholder :: (IsSLComposeServiceViewController slComposeServiceViewController, IsNSString value) => slComposeServiceViewController -> value -> IO ()
setPlaceholder slComposeServiceViewController value =
  sendMessage slComposeServiceViewController setPlaceholderSelector (toNSString value)

-- | @- charactersRemaining@
charactersRemaining :: IsSLComposeServiceViewController slComposeServiceViewController => slComposeServiceViewController -> IO (Id NSNumber)
charactersRemaining slComposeServiceViewController =
  sendMessage slComposeServiceViewController charactersRemainingSelector

-- | @- setCharactersRemaining:@
setCharactersRemaining :: (IsSLComposeServiceViewController slComposeServiceViewController, IsNSNumber value) => slComposeServiceViewController -> value -> IO ()
setCharactersRemaining slComposeServiceViewController value =
  sendMessage slComposeServiceViewController setCharactersRemainingSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentationAnimationDidFinish@
presentationAnimationDidFinishSelector :: Selector '[] ()
presentationAnimationDidFinishSelector = mkSelector "presentationAnimationDidFinish"

-- | @Selector@ for @didSelectPost@
didSelectPostSelector :: Selector '[] ()
didSelectPostSelector = mkSelector "didSelectPost"

-- | @Selector@ for @didSelectCancel@
didSelectCancelSelector :: Selector '[] ()
didSelectCancelSelector = mkSelector "didSelectCancel"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @isContentValid@
isContentValidSelector :: Selector '[] Bool
isContentValidSelector = mkSelector "isContentValid"

-- | @Selector@ for @validateContent@
validateContentSelector :: Selector '[] ()
validateContentSelector = mkSelector "validateContent"

-- | @Selector@ for @textView@
textViewSelector :: Selector '[] (Id NSTextView)
textViewSelector = mkSelector "textView"

-- | @Selector@ for @contentText@
contentTextSelector :: Selector '[] (Id NSString)
contentTextSelector = mkSelector "contentText"

-- | @Selector@ for @placeholder@
placeholderSelector :: Selector '[] (Id NSString)
placeholderSelector = mkSelector "placeholder"

-- | @Selector@ for @setPlaceholder:@
setPlaceholderSelector :: Selector '[Id NSString] ()
setPlaceholderSelector = mkSelector "setPlaceholder:"

-- | @Selector@ for @charactersRemaining@
charactersRemainingSelector :: Selector '[] (Id NSNumber)
charactersRemainingSelector = mkSelector "charactersRemaining"

-- | @Selector@ for @setCharactersRemaining:@
setCharactersRemainingSelector :: Selector '[Id NSNumber] ()
setCharactersRemainingSelector = mkSelector "setCharactersRemaining:"

