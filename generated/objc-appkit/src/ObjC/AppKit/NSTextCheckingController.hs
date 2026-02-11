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
  , spellCheckerDocumentTag
  , setSpellCheckerDocumentTag
  , initWithClientSelector
  , initSelector
  , invalidateSelector
  , didChangeTextInRangeSelector
  , insertedTextInRangeSelector
  , didChangeSelectedRangeSelector
  , considerTextCheckingForRangeSelector
  , checkTextInRange_types_optionsSelector
  , checkTextInSelectionSelector
  , checkTextInDocumentSelector
  , orderFrontSubstitutionsPanelSelector
  , checkSpellingSelector
  , showGuessPanelSelector
  , changeSpellingSelector
  , ignoreSpellingSelector
  , updateCandidatesSelector
  , validAnnotationsSelector
  , menuAtIndex_clickedOnSelection_effectiveRangeSelector
  , spellCheckerDocumentTagSelector
  , setSpellCheckerDocumentTagSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithClient:@
initWithClient :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO (Id NSTextCheckingController)
initWithClient nsTextCheckingController  client =
  sendMsg nsTextCheckingController (mkSelector "initWithClient:") (retPtr retVoid) [argPtr (castPtr (unRawId client) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO (Id NSTextCheckingController)
init_ nsTextCheckingController  =
  sendMsg nsTextCheckingController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- invalidate@
invalidate :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO ()
invalidate nsTextCheckingController  =
  sendMsg nsTextCheckingController (mkSelector "invalidate") retVoid []

-- | @- didChangeTextInRange:@
didChangeTextInRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> NSRange -> IO ()
didChangeTextInRange nsTextCheckingController  range =
  sendMsg nsTextCheckingController (mkSelector "didChangeTextInRange:") retVoid [argNSRange range]

-- | @- insertedTextInRange:@
insertedTextInRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> NSRange -> IO ()
insertedTextInRange nsTextCheckingController  range =
  sendMsg nsTextCheckingController (mkSelector "insertedTextInRange:") retVoid [argNSRange range]

-- | @- didChangeSelectedRange@
didChangeSelectedRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO ()
didChangeSelectedRange nsTextCheckingController  =
  sendMsg nsTextCheckingController (mkSelector "didChangeSelectedRange") retVoid []

-- | @- considerTextCheckingForRange:@
considerTextCheckingForRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> NSRange -> IO ()
considerTextCheckingForRange nsTextCheckingController  range =
  sendMsg nsTextCheckingController (mkSelector "considerTextCheckingForRange:") retVoid [argNSRange range]

-- | @- checkTextInRange:types:options:@
checkTextInRange_types_options :: (IsNSTextCheckingController nsTextCheckingController, IsNSDictionary options) => nsTextCheckingController -> NSRange -> CULong -> options -> IO ()
checkTextInRange_types_options nsTextCheckingController  range checkingTypes options =
withObjCPtr options $ \raw_options ->
    sendMsg nsTextCheckingController (mkSelector "checkTextInRange:types:options:") retVoid [argNSRange range, argCULong (fromIntegral checkingTypes), argPtr (castPtr raw_options :: Ptr ())]

-- | @- checkTextInSelection:@
checkTextInSelection :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
checkTextInSelection nsTextCheckingController  sender =
  sendMsg nsTextCheckingController (mkSelector "checkTextInSelection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- checkTextInDocument:@
checkTextInDocument :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
checkTextInDocument nsTextCheckingController  sender =
  sendMsg nsTextCheckingController (mkSelector "checkTextInDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanel :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
orderFrontSubstitutionsPanel nsTextCheckingController  sender =
  sendMsg nsTextCheckingController (mkSelector "orderFrontSubstitutionsPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- checkSpelling:@
checkSpelling :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
checkSpelling nsTextCheckingController  sender =
  sendMsg nsTextCheckingController (mkSelector "checkSpelling:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- showGuessPanel:@
showGuessPanel :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
showGuessPanel nsTextCheckingController  sender =
  sendMsg nsTextCheckingController (mkSelector "showGuessPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeSpelling:@
changeSpelling :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
changeSpelling nsTextCheckingController  sender =
  sendMsg nsTextCheckingController (mkSelector "changeSpelling:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- ignoreSpelling:@
ignoreSpelling :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> RawId -> IO ()
ignoreSpelling nsTextCheckingController  sender =
  sendMsg nsTextCheckingController (mkSelector "ignoreSpelling:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- updateCandidates@
updateCandidates :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO ()
updateCandidates nsTextCheckingController  =
  sendMsg nsTextCheckingController (mkSelector "updateCandidates") retVoid []

-- | @- validAnnotations@
validAnnotations :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO (Id NSArray)
validAnnotations nsTextCheckingController  =
  sendMsg nsTextCheckingController (mkSelector "validAnnotations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- menuAtIndex:clickedOnSelection:effectiveRange:@
menuAtIndex_clickedOnSelection_effectiveRange :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> CULong -> Bool -> Ptr NSRange -> IO (Id NSMenu)
menuAtIndex_clickedOnSelection_effectiveRange nsTextCheckingController  location clickedOnSelection effectiveRange =
  sendMsg nsTextCheckingController (mkSelector "menuAtIndex:clickedOnSelection:effectiveRange:") (retPtr retVoid) [argCULong (fromIntegral location), argCULong (if clickedOnSelection then 1 else 0), argPtr effectiveRange] >>= retainedObject . castPtr

-- | @- spellCheckerDocumentTag@
spellCheckerDocumentTag :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> IO CLong
spellCheckerDocumentTag nsTextCheckingController  =
  sendMsg nsTextCheckingController (mkSelector "spellCheckerDocumentTag") retCLong []

-- | @- setSpellCheckerDocumentTag:@
setSpellCheckerDocumentTag :: IsNSTextCheckingController nsTextCheckingController => nsTextCheckingController -> CLong -> IO ()
setSpellCheckerDocumentTag nsTextCheckingController  value =
  sendMsg nsTextCheckingController (mkSelector "setSpellCheckerDocumentTag:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithClient:@
initWithClientSelector :: Selector
initWithClientSelector = mkSelector "initWithClient:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @didChangeTextInRange:@
didChangeTextInRangeSelector :: Selector
didChangeTextInRangeSelector = mkSelector "didChangeTextInRange:"

-- | @Selector@ for @insertedTextInRange:@
insertedTextInRangeSelector :: Selector
insertedTextInRangeSelector = mkSelector "insertedTextInRange:"

-- | @Selector@ for @didChangeSelectedRange@
didChangeSelectedRangeSelector :: Selector
didChangeSelectedRangeSelector = mkSelector "didChangeSelectedRange"

-- | @Selector@ for @considerTextCheckingForRange:@
considerTextCheckingForRangeSelector :: Selector
considerTextCheckingForRangeSelector = mkSelector "considerTextCheckingForRange:"

-- | @Selector@ for @checkTextInRange:types:options:@
checkTextInRange_types_optionsSelector :: Selector
checkTextInRange_types_optionsSelector = mkSelector "checkTextInRange:types:options:"

-- | @Selector@ for @checkTextInSelection:@
checkTextInSelectionSelector :: Selector
checkTextInSelectionSelector = mkSelector "checkTextInSelection:"

-- | @Selector@ for @checkTextInDocument:@
checkTextInDocumentSelector :: Selector
checkTextInDocumentSelector = mkSelector "checkTextInDocument:"

-- | @Selector@ for @orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanelSelector :: Selector
orderFrontSubstitutionsPanelSelector = mkSelector "orderFrontSubstitutionsPanel:"

-- | @Selector@ for @checkSpelling:@
checkSpellingSelector :: Selector
checkSpellingSelector = mkSelector "checkSpelling:"

-- | @Selector@ for @showGuessPanel:@
showGuessPanelSelector :: Selector
showGuessPanelSelector = mkSelector "showGuessPanel:"

-- | @Selector@ for @changeSpelling:@
changeSpellingSelector :: Selector
changeSpellingSelector = mkSelector "changeSpelling:"

-- | @Selector@ for @ignoreSpelling:@
ignoreSpellingSelector :: Selector
ignoreSpellingSelector = mkSelector "ignoreSpelling:"

-- | @Selector@ for @updateCandidates@
updateCandidatesSelector :: Selector
updateCandidatesSelector = mkSelector "updateCandidates"

-- | @Selector@ for @validAnnotations@
validAnnotationsSelector :: Selector
validAnnotationsSelector = mkSelector "validAnnotations"

-- | @Selector@ for @menuAtIndex:clickedOnSelection:effectiveRange:@
menuAtIndex_clickedOnSelection_effectiveRangeSelector :: Selector
menuAtIndex_clickedOnSelection_effectiveRangeSelector = mkSelector "menuAtIndex:clickedOnSelection:effectiveRange:"

-- | @Selector@ for @spellCheckerDocumentTag@
spellCheckerDocumentTagSelector :: Selector
spellCheckerDocumentTagSelector = mkSelector "spellCheckerDocumentTag"

-- | @Selector@ for @setSpellCheckerDocumentTag:@
setSpellCheckerDocumentTagSelector :: Selector
setSpellCheckerDocumentTagSelector = mkSelector "setSpellCheckerDocumentTag:"

