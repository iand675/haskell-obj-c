{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWCollaborationShareOptions
--
-- represents the state of the collaboration options for the document.
--
-- SWCollaborationShareOptions contains the SWCollaborationOptionsGorups that are available for the collaboration as well as a string, provided by the client, that summarizes the state of the selected options.
--
-- Generated bindings for @SWCollaborationShareOptions@.
module ObjC.SharedWithYouCore.SWCollaborationShareOptions
  ( SWCollaborationShareOptions
  , IsSWCollaborationShareOptions(..)
  , initWithOptionsGroups_summary
  , initWithOptionsGroups
  , shareOptionsWithOptionsGroups_summary
  , shareOptionsWithOptionsGroups
  , init_
  , initWithCoder
  , optionsGroups
  , setOptionsGroups
  , summary
  , setSummary
  , initWithOptionsGroups_summarySelector
  , initWithOptionsGroupsSelector
  , shareOptionsWithOptionsGroups_summarySelector
  , shareOptionsWithOptionsGroupsSelector
  , initSelector
  , initWithCoderSelector
  , optionsGroupsSelector
  , setOptionsGroupsSelector
  , summarySelector
  , setSummarySelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a shareOptions object to represent the available collaboration options for the document and a summary of the selected options
--
-- @optionsGroups@ — SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- @summary@ — localized string to summarize the selected collaboration options
--
-- ObjC selector: @- initWithOptionsGroups:summary:@
initWithOptionsGroups_summary :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSArray optionsGroups, IsNSString summary) => swCollaborationShareOptions -> optionsGroups -> summary -> IO (Id SWCollaborationShareOptions)
initWithOptionsGroups_summary swCollaborationShareOptions  optionsGroups summary =
withObjCPtr optionsGroups $ \raw_optionsGroups ->
  withObjCPtr summary $ \raw_summary ->
      sendMsg swCollaborationShareOptions (mkSelector "initWithOptionsGroups:summary:") (retPtr retVoid) [argPtr (castPtr raw_optionsGroups :: Ptr ()), argPtr (castPtr raw_summary :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a shareOptions object to represent the available collaboration options for the document and the default summary string "Share Options"
--
-- @optionsGroups@ — SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @- initWithOptionsGroups:@
initWithOptionsGroups :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSArray optionsGroups) => swCollaborationShareOptions -> optionsGroups -> IO (Id SWCollaborationShareOptions)
initWithOptionsGroups swCollaborationShareOptions  optionsGroups =
withObjCPtr optionsGroups $ \raw_optionsGroups ->
    sendMsg swCollaborationShareOptions (mkSelector "initWithOptionsGroups:") (retPtr retVoid) [argPtr (castPtr raw_optionsGroups :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a shareOptions object to represent the available collaboration options for the document and a summary of the selected options
--
-- @optionsGroups@ — SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- @summary@ — localized string to summarize the selected collaboration options
--
-- ObjC selector: @+ shareOptionsWithOptionsGroups:summary:@
shareOptionsWithOptionsGroups_summary :: (IsNSArray optionsGroups, IsNSString summary) => optionsGroups -> summary -> IO (Id SWCollaborationShareOptions)
shareOptionsWithOptionsGroups_summary optionsGroups summary =
  do
    cls' <- getRequiredClass "SWCollaborationShareOptions"
    withObjCPtr optionsGroups $ \raw_optionsGroups ->
      withObjCPtr summary $ \raw_summary ->
        sendClassMsg cls' (mkSelector "shareOptionsWithOptionsGroups:summary:") (retPtr retVoid) [argPtr (castPtr raw_optionsGroups :: Ptr ()), argPtr (castPtr raw_summary :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a shareOptions object to represent the available collaboration options for the document and a summary of the selected options
--
-- @optionsGroups@ — SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @+ shareOptionsWithOptionsGroups:@
shareOptionsWithOptionsGroups :: IsNSArray optionsGroups => optionsGroups -> IO (Id SWCollaborationShareOptions)
shareOptionsWithOptionsGroups optionsGroups =
  do
    cls' <- getRequiredClass "SWCollaborationShareOptions"
    withObjCPtr optionsGroups $ \raw_optionsGroups ->
      sendClassMsg cls' (mkSelector "shareOptionsWithOptionsGroups:") (retPtr retVoid) [argPtr (castPtr raw_optionsGroups :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsSWCollaborationShareOptions swCollaborationShareOptions => swCollaborationShareOptions -> IO (Id SWCollaborationShareOptions)
init_ swCollaborationShareOptions  =
  sendMsg swCollaborationShareOptions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSCoder coder) => swCollaborationShareOptions -> coder -> IO (Id SWCollaborationShareOptions)
initWithCoder swCollaborationShareOptions  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg swCollaborationShareOptions (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @- optionsGroups@
optionsGroups :: IsSWCollaborationShareOptions swCollaborationShareOptions => swCollaborationShareOptions -> IO (Id NSArray)
optionsGroups swCollaborationShareOptions  =
  sendMsg swCollaborationShareOptions (mkSelector "optionsGroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @- setOptionsGroups:@
setOptionsGroups :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSArray value) => swCollaborationShareOptions -> value -> IO ()
setOptionsGroups swCollaborationShareOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationShareOptions (mkSelector "setOptionsGroups:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Localized string to summarize the selected collaboration options. If nil, "Share Options" will be displayed by default.
--
-- ObjC selector: @- summary@
summary :: IsSWCollaborationShareOptions swCollaborationShareOptions => swCollaborationShareOptions -> IO (Id NSString)
summary swCollaborationShareOptions  =
  sendMsg swCollaborationShareOptions (mkSelector "summary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized string to summarize the selected collaboration options. If nil, "Share Options" will be displayed by default.
--
-- ObjC selector: @- setSummary:@
setSummary :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSString value) => swCollaborationShareOptions -> value -> IO ()
setSummary swCollaborationShareOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg swCollaborationShareOptions (mkSelector "setSummary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptionsGroups:summary:@
initWithOptionsGroups_summarySelector :: Selector
initWithOptionsGroups_summarySelector = mkSelector "initWithOptionsGroups:summary:"

-- | @Selector@ for @initWithOptionsGroups:@
initWithOptionsGroupsSelector :: Selector
initWithOptionsGroupsSelector = mkSelector "initWithOptionsGroups:"

-- | @Selector@ for @shareOptionsWithOptionsGroups:summary:@
shareOptionsWithOptionsGroups_summarySelector :: Selector
shareOptionsWithOptionsGroups_summarySelector = mkSelector "shareOptionsWithOptionsGroups:summary:"

-- | @Selector@ for @shareOptionsWithOptionsGroups:@
shareOptionsWithOptionsGroupsSelector :: Selector
shareOptionsWithOptionsGroupsSelector = mkSelector "shareOptionsWithOptionsGroups:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @optionsGroups@
optionsGroupsSelector :: Selector
optionsGroupsSelector = mkSelector "optionsGroups"

-- | @Selector@ for @setOptionsGroups:@
setOptionsGroupsSelector :: Selector
setOptionsGroupsSelector = mkSelector "setOptionsGroups:"

-- | @Selector@ for @summary@
summarySelector :: Selector
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector
setSummarySelector = mkSelector "setSummary:"

