{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithCoderSelector
  , initWithOptionsGroupsSelector
  , initWithOptionsGroups_summarySelector
  , optionsGroupsSelector
  , setOptionsGroupsSelector
  , setSummarySelector
  , shareOptionsWithOptionsGroupsSelector
  , shareOptionsWithOptionsGroups_summarySelector
  , summarySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithOptionsGroups_summary swCollaborationShareOptions optionsGroups summary =
  sendOwnedMessage swCollaborationShareOptions initWithOptionsGroups_summarySelector (toNSArray optionsGroups) (toNSString summary)

-- | Initializes a shareOptions object to represent the available collaboration options for the document and the default summary string "Share Options"
--
-- @optionsGroups@ — SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @- initWithOptionsGroups:@
initWithOptionsGroups :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSArray optionsGroups) => swCollaborationShareOptions -> optionsGroups -> IO (Id SWCollaborationShareOptions)
initWithOptionsGroups swCollaborationShareOptions optionsGroups =
  sendOwnedMessage swCollaborationShareOptions initWithOptionsGroupsSelector (toNSArray optionsGroups)

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
    sendClassMessage cls' shareOptionsWithOptionsGroups_summarySelector (toNSArray optionsGroups) (toNSString summary)

-- | Creates a shareOptions object to represent the available collaboration options for the document and a summary of the selected options
--
-- @optionsGroups@ — SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @+ shareOptionsWithOptionsGroups:@
shareOptionsWithOptionsGroups :: IsNSArray optionsGroups => optionsGroups -> IO (Id SWCollaborationShareOptions)
shareOptionsWithOptionsGroups optionsGroups =
  do
    cls' <- getRequiredClass "SWCollaborationShareOptions"
    sendClassMessage cls' shareOptionsWithOptionsGroupsSelector (toNSArray optionsGroups)

-- | @- init@
init_ :: IsSWCollaborationShareOptions swCollaborationShareOptions => swCollaborationShareOptions -> IO (Id SWCollaborationShareOptions)
init_ swCollaborationShareOptions =
  sendOwnedMessage swCollaborationShareOptions initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSCoder coder) => swCollaborationShareOptions -> coder -> IO (Id SWCollaborationShareOptions)
initWithCoder swCollaborationShareOptions coder =
  sendOwnedMessage swCollaborationShareOptions initWithCoderSelector (toNSCoder coder)

-- | SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @- optionsGroups@
optionsGroups :: IsSWCollaborationShareOptions swCollaborationShareOptions => swCollaborationShareOptions -> IO (Id NSArray)
optionsGroups swCollaborationShareOptions =
  sendMessage swCollaborationShareOptions optionsGroupsSelector

-- | SWCollaborationOptionsGroups to customize how the collaboration will be shared
--
-- ObjC selector: @- setOptionsGroups:@
setOptionsGroups :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSArray value) => swCollaborationShareOptions -> value -> IO ()
setOptionsGroups swCollaborationShareOptions value =
  sendMessage swCollaborationShareOptions setOptionsGroupsSelector (toNSArray value)

-- | Localized string to summarize the selected collaboration options. If nil, "Share Options" will be displayed by default.
--
-- ObjC selector: @- summary@
summary :: IsSWCollaborationShareOptions swCollaborationShareOptions => swCollaborationShareOptions -> IO (Id NSString)
summary swCollaborationShareOptions =
  sendMessage swCollaborationShareOptions summarySelector

-- | Localized string to summarize the selected collaboration options. If nil, "Share Options" will be displayed by default.
--
-- ObjC selector: @- setSummary:@
setSummary :: (IsSWCollaborationShareOptions swCollaborationShareOptions, IsNSString value) => swCollaborationShareOptions -> value -> IO ()
setSummary swCollaborationShareOptions value =
  sendMessage swCollaborationShareOptions setSummarySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptionsGroups:summary:@
initWithOptionsGroups_summarySelector :: Selector '[Id NSArray, Id NSString] (Id SWCollaborationShareOptions)
initWithOptionsGroups_summarySelector = mkSelector "initWithOptionsGroups:summary:"

-- | @Selector@ for @initWithOptionsGroups:@
initWithOptionsGroupsSelector :: Selector '[Id NSArray] (Id SWCollaborationShareOptions)
initWithOptionsGroupsSelector = mkSelector "initWithOptionsGroups:"

-- | @Selector@ for @shareOptionsWithOptionsGroups:summary:@
shareOptionsWithOptionsGroups_summarySelector :: Selector '[Id NSArray, Id NSString] (Id SWCollaborationShareOptions)
shareOptionsWithOptionsGroups_summarySelector = mkSelector "shareOptionsWithOptionsGroups:summary:"

-- | @Selector@ for @shareOptionsWithOptionsGroups:@
shareOptionsWithOptionsGroupsSelector :: Selector '[Id NSArray] (Id SWCollaborationShareOptions)
shareOptionsWithOptionsGroupsSelector = mkSelector "shareOptionsWithOptionsGroups:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWCollaborationShareOptions)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SWCollaborationShareOptions)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @optionsGroups@
optionsGroupsSelector :: Selector '[] (Id NSArray)
optionsGroupsSelector = mkSelector "optionsGroups"

-- | @Selector@ for @setOptionsGroups:@
setOptionsGroupsSelector :: Selector '[Id NSArray] ()
setOptionsGroupsSelector = mkSelector "setOptionsGroups:"

-- | @Selector@ for @summary@
summarySelector :: Selector '[] (Id NSString)
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector '[Id NSString] ()
setSummarySelector = mkSelector "setSummary:"

