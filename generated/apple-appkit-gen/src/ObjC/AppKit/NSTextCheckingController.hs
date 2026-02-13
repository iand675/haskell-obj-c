{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextCheckingController@.
module ObjC.AppKit.NSTextCheckingController
  ( NSTextCheckingController
  , IsNSTextCheckingController(..)
  , initWithClient
  , init_
  , invalidate
  , didChangeTextInRange
  , insertedTextInRange
  , didChangeSelectedRange
  , considerTextCheckingForRange
  , checkTextInRange_types_options
  , checkTextInSelection
  , checkTextInDocument
  , orderFrontSubstitutionsPanel
  , checkSpelling
  , showGuessPanel
  , changeSpelling
  , ignoreSpelling
  , updateCandidates
  , validAnnotations
  , menuAtIndex_clickedOnSelection_effectiveRange
  , client
  , spellCheckerDocumentTag
  , setSpellCheckerDocumentTag
  , changeSpellingSelector
  , checkSpellingSelector
  , checkTextInDocumentSelector
  , checkTextInRange_types_optionsSelector
  , checkTextInSelectionSelector
  , clientSelector
  , considerTextCheckingForRangeSelector
  , didChangeSelectedRangeSelector
  , didChangeTextInRangeSelector
  , ignoreSpellingSelector
  , initSelector
  , initWithClientSelector
  , insertedTextInRangeSelector
  , invalidateSelector
  , menuAtIndex_clickedOnSelection_effectiveRangeSelector
  , orderFrontSubstitutionsPanelSelector
  , setSpellCheckerDocumentTagSelector
  , showGuessPanelSelector
  , spellCheckerDocumentTagSelector
  , updateCandidatesSelector
  , validAnnotationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithClient:@
initWithClient :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO (Id NSTextCheckingController)
initWithClient nsTextCheckingController client =
  sendOwnedMessage nsTextCheckingController initWithClientSelector client

-- | @- init@
init_ :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO (Id NSTextCheckingController)
init_ nsTextCheckingController =
  sendOwnedMessage nsTextCheckingController initSelector

-- | @- invalidate@
invalidate :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO ()
invalidate nsTextCheckingController =
  sendMessage nsTextCheckingController invalidateSelector

-- | @- didChangeTextInRange:@
didChangeTextInRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> NSRange -> IO ()
didChangeTextInRange nsTextCheckingController range =
  sendMessage nsTextCheckingController didChangeTextInRangeSelector range

-- | @- insertedTextInRange:@
insertedTextInRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> NSRange -> IO ()
insertedTextInRange nsTextCheckingController range =
  sendMessage nsTextCheckingController insertedTextInRangeSelector range

-- | @- didChangeSelectedRange@
didChangeSelectedRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO ()
didChangeSelectedRange nsTextCheckingController =
  sendMessage nsTextCheckingController didChangeSelectedRangeSelector

-- | @- considerTextCheckingForRange:@
considerTextCheckingForRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> NSRange -> IO ()
considerTextCheckingForRange nsTextCheckingController range =
  sendMessage nsTextCheckingController considerTextCheckingForRangeSelector range

-- | @- checkTextInRange:types:options:@
checkTextInRange_types_options :: (IsNSTextCheckingController nsTextCheckingController, IsNSDictionary options) => nsTextCheckingController -> NSRange -> CULong -> options -> IO ()
checkTextInRange_types_options nsTextCheckingController range checkingTypes options =
  sendMessage nsTextCheckingController checkTextInRange_types_optionsSelector range checkingTypes (toNSDictionary options)

-- | @- checkTextInSelection:@
checkTextInSelection :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
checkTextInSelection nsTextCheckingController sender =
  sendMessage nsTextCheckingController checkTextInSelectionSelector sender

-- | @- checkTextInDocument:@
checkTextInDocument :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
checkTextInDocument nsTextCheckingController sender =
  sendMessage nsTextCheckingController checkTextInDocumentSelector sender

-- | @- orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanel :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
orderFrontSubstitutionsPanel nsTextCheckingController sender =
  sendMessage nsTextCheckingController orderFrontSubstitutionsPanelSelector sender

-- | @- checkSpelling:@
checkSpelling :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
checkSpelling nsTextCheckingController sender =
  sendMessage nsTextCheckingController checkSpellingSelector sender

-- | @- showGuessPanel:@
showGuessPanel :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
showGuessPanel nsTextCheckingController sender =
  sendMessage nsTextCheckingController showGuessPanelSelector sender

-- | @- changeSpelling:@
changeSpelling :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
changeSpelling nsTextCheckingController sender =
  sendMessage nsTextCheckingController changeSpellingSelector sender

-- | @- ignoreSpelling:@
ignoreSpelling :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
ignoreSpelling nsTextCheckingController sender =
  sendMessage nsTextCheckingController ignoreSpellingSelector sender

-- | @- updateCandidates@
updateCandidates :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO ()
updateCandidates nsTextCheckingController =
  sendMessage nsTextCheckingController updateCandidatesSelector

-- | @- validAnnotations@
validAnnotations :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO (Id NSArray)
validAnnotations nsTextCheckingController =
  sendMessage nsTextCheckingController validAnnotationsSelector

-- | @- menuAtIndex:clickedOnSelection:effectiveRange:@
menuAtIndex_clickedOnSelection_effectiveRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> CULong -> Bool -> Ptr NSRange -> IO (Id NSMenu)
menuAtIndex_clickedOnSelection_effectiveRange nsTextCheckingController location clickedOnSelection effectiveRange =
  sendMessage nsTextCheckingController menuAtIndex_clickedOnSelection_effectiveRangeSelector location clickedOnSelection effectiveRange

-- | @- client@
client :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO RawId
client nsTextCheckingController =
  sendMessage nsTextCheckingController clientSelector

-- | @- spellCheckerDocumentTag@
spellCheckerDocumentTag :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO CLong
spellCheckerDocumentTag nsTextCheckingController =
  sendMessage nsTextCheckingController spellCheckerDocumentTagSelector

-- | @- setSpellCheckerDocumentTag:@
setSpellCheckerDocumentTag :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> CLong -> IO ()
setSpellCheckerDocumentTag nsTextCheckingController value =
  sendMessage nsTextCheckingController setSpellCheckerDocumentTagSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithClient:@
initWithClientSelector :: Selector '[RawId] (Id NSTextCheckingController)
initWithClientSelector = mkSelector "initWithClient:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextCheckingController)
initSelector = mkSelector "init"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @didChangeTextInRange:@
didChangeTextInRangeSelector :: Selector '[NSRange] ()
didChangeTextInRangeSelector = mkSelector "didChangeTextInRange:"

-- | @Selector@ for @insertedTextInRange:@
insertedTextInRangeSelector :: Selector '[NSRange] ()
insertedTextInRangeSelector = mkSelector "insertedTextInRange:"

-- | @Selector@ for @didChangeSelectedRange@
didChangeSelectedRangeSelector :: Selector '[] ()
didChangeSelectedRangeSelector = mkSelector "didChangeSelectedRange"

-- | @Selector@ for @considerTextCheckingForRange:@
considerTextCheckingForRangeSelector :: Selector '[NSRange] ()
considerTextCheckingForRangeSelector = mkSelector "considerTextCheckingForRange:"

-- | @Selector@ for @checkTextInRange:types:options:@
checkTextInRange_types_optionsSelector :: Selector '[NSRange, CULong, Id NSDictionary] ()
checkTextInRange_types_optionsSelector = mkSelector "checkTextInRange:types:options:"

-- | @Selector@ for @checkTextInSelection:@
checkTextInSelectionSelector :: Selector '[RawId] ()
checkTextInSelectionSelector = mkSelector "checkTextInSelection:"

-- | @Selector@ for @checkTextInDocument:@
checkTextInDocumentSelector :: Selector '[RawId] ()
checkTextInDocumentSelector = mkSelector "checkTextInDocument:"

-- | @Selector@ for @orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanelSelector :: Selector '[RawId] ()
orderFrontSubstitutionsPanelSelector = mkSelector "orderFrontSubstitutionsPanel:"

-- | @Selector@ for @checkSpelling:@
checkSpellingSelector :: Selector '[RawId] ()
checkSpellingSelector = mkSelector "checkSpelling:"

-- | @Selector@ for @showGuessPanel:@
showGuessPanelSelector :: Selector '[RawId] ()
showGuessPanelSelector = mkSelector "showGuessPanel:"

-- | @Selector@ for @changeSpelling:@
changeSpellingSelector :: Selector '[RawId] ()
changeSpellingSelector = mkSelector "changeSpelling:"

-- | @Selector@ for @ignoreSpelling:@
ignoreSpellingSelector :: Selector '[RawId] ()
ignoreSpellingSelector = mkSelector "ignoreSpelling:"

-- | @Selector@ for @updateCandidates@
updateCandidatesSelector :: Selector '[] ()
updateCandidatesSelector = mkSelector "updateCandidates"

-- | @Selector@ for @validAnnotations@
validAnnotationsSelector :: Selector '[] (Id NSArray)
validAnnotationsSelector = mkSelector "validAnnotations"

-- | @Selector@ for @menuAtIndex:clickedOnSelection:effectiveRange:@
menuAtIndex_clickedOnSelection_effectiveRangeSelector :: Selector '[CULong, Bool, Ptr NSRange] (Id NSMenu)
menuAtIndex_clickedOnSelection_effectiveRangeSelector = mkSelector "menuAtIndex:clickedOnSelection:effectiveRange:"

-- | @Selector@ for @client@
clientSelector :: Selector '[] RawId
clientSelector = mkSelector "client"

-- | @Selector@ for @spellCheckerDocumentTag@
spellCheckerDocumentTagSelector :: Selector '[] CLong
spellCheckerDocumentTagSelector = mkSelector "spellCheckerDocumentTag"

-- | @Selector@ for @setSpellCheckerDocumentTag:@
setSpellCheckerDocumentTagSelector :: Selector '[CLong] ()
setSpellCheckerDocumentTagSelector = mkSelector "setSpellCheckerDocumentTag:"

