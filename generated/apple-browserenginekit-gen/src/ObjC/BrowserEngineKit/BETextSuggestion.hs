{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BETextSuggestion@.
module ObjC.BrowserEngineKit.BETextSuggestion
  ( BETextSuggestion
  , IsBETextSuggestion(..)
  , initWithInputText
  , new
  , init_
  , inputText
  , initSelector
  , initWithInputTextSelector
  , inputTextSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new text suggestion with the given input text.
--
-- ObjC selector: @- initWithInputText:@
initWithInputText :: (IsBETextSuggestion beTextSuggestion, IsNSString inputText) => beTextSuggestion -> inputText -> IO (Id BETextSuggestion)
initWithInputText beTextSuggestion inputText =
  sendOwnedMessage beTextSuggestion initWithInputTextSelector (toNSString inputText)

-- | @- new@
new :: IsBETextSuggestion beTextSuggestion => beTextSuggestion -> IO (Id BETextSuggestion)
new beTextSuggestion =
  sendOwnedMessage beTextSuggestion newSelector

-- | @- init@
init_ :: IsBETextSuggestion beTextSuggestion => beTextSuggestion -> IO (Id BETextSuggestion)
init_ beTextSuggestion =
  sendOwnedMessage beTextSuggestion initSelector

-- | Text that will be inserted into the document when the user chooses the suggestion.
--
-- ObjC selector: @- inputText@
inputText :: IsBETextSuggestion beTextSuggestion => beTextSuggestion -> IO (Id NSString)
inputText beTextSuggestion =
  sendMessage beTextSuggestion inputTextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInputText:@
initWithInputTextSelector :: Selector '[Id NSString] (Id BETextSuggestion)
initWithInputTextSelector = mkSelector "initWithInputText:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BETextSuggestion)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BETextSuggestion)
initSelector = mkSelector "init"

-- | @Selector@ for @inputText@
inputTextSelector :: Selector '[] (Id NSString)
inputTextSelector = mkSelector "inputText"

