{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogStore
--
-- A set of entries from the unified logging system. Instances represent a fixed range of entries and may be backed by a logarchive or the Mac's local store.
--
-- Entries in OSLogStore objects are used by OSLogEnumerator instances; one store can support multiple OSLogEnumerator instances concurrently.
--
-- Generated bindings for @OSLogStore@.
module ObjC.OSLog.OSLogStore
  ( OSLogStore
  , IsOSLogStore(..)
  , localStoreAndReturnError
  , storeWithScope_error
  , storeWithURL_error
  , init_
  , entriesEnumeratorWithOptions_position_predicate_error
  , entriesEnumeratorAndReturnError
  , positionWithDate
  , positionWithTimeIntervalSinceEnd
  , positionWithTimeIntervalSinceLatestBoot
  , localStoreAndReturnErrorSelector
  , storeWithScope_errorSelector
  , storeWithURL_errorSelector
  , initSelector
  , entriesEnumeratorWithOptions_position_predicate_errorSelector
  , entriesEnumeratorAndReturnErrorSelector
  , positionWithDateSelector
  , positionWithTimeIntervalSinceEndSelector
  , positionWithTimeIntervalSinceLatestBootSelector

  -- * Enum types
  , OSLogEnumeratorOptions(OSLogEnumeratorOptions)
  , pattern OSLogEnumeratorReverse
  , OSLogStoreScope(OSLogStoreScope)
  , pattern OSLogStoreSystem
  , pattern OSLogStoreCurrentProcessIdentifier

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

import ObjC.OSLog.Internal.Classes
import ObjC.OSLog.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | localStoreAndReturnError
--
-- Create an OSLogStore representing the Mac's local store.
--
-- @error@ — If initialization is unsuccessful --- for example, this process does not have access to local logs --- return nil and set this parameter to a pointer to an error object describing the reason.
--
-- This enables processing of a sequence of logs as of the particular point in time when this object is created.
--
-- Gaining access to the local unified logging system requires permission from the system. The caller must be run by an admin account.
--
-- ObjC selector: @+ localStoreAndReturnError:@
localStoreAndReturnError :: IsNSError error_ => error_ -> IO (Id OSLogStore)
localStoreAndReturnError error_ =
  do
    cls' <- getRequiredClass "OSLogStore"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "localStoreAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | storeWithScope
--
-- Create an OSLogStore for a subset of entries in the local store.
--
-- @scope@ — The kind of subset the OSLogStore is for.
--
-- @error@ — If initialization is unsuccessful, return nil and set this parameter to a pointer to an error object that describes the reason.
--
-- ObjC selector: @+ storeWithScope:error:@
storeWithScope_error :: IsNSError error_ => OSLogStoreScope -> error_ -> IO (Id OSLogStore)
storeWithScope_error scope error_ =
  do
    cls' <- getRequiredClass "OSLogStore"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "storeWithScope:error:") (retPtr retVoid) [argCLong (coerce scope), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | storeWithURL
--
-- Create an OSLogStore based on a logarchive.
--
-- @url@ — The path identifying a logarchive to be read.
--
-- @error@ — If initialization is unsuccessful --- for example, the path is not to a valid logarchive or the logarchive is not compatible because it is from a newer version --- return nil and set this parameter to a pointer to an error object that describes the reason.
--
-- ObjC selector: @+ storeWithURL:error:@
storeWithURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id OSLogStore)
storeWithURL_error url error_ =
  do
    cls' <- getRequiredClass "OSLogStore"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "storeWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsOSLogStore osLogStore => osLogStore -> IO (Id OSLogStore)
init_ osLogStore  =
  sendMsg osLogStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | entriesEnumeratorWithOptions
--
-- Return an OSLogEnumerator object based on an underlying store. This object represents the sequence of entries for the store. OSLogStore. Additional parameters control which entries are yielded and their order.
--
-- @options@ — Control the direction of iteration.
--
-- @position@ — Where to start iteration. If nil, depend on the direction of the iteration: if forwards, start with the earliest entry; if reverse, start with the latest entry.
--
-- @predicate@ — A predicate that filters which entries are in the sequence. If this is nil, yield all entries.
--
-- @error@ — If the enumerator cannot be set up --- for example, the predicate has an unrecognized key --- return nil and set this to a pointer to an error object that describes the reason.
--
-- ObjC selector: @- entriesEnumeratorWithOptions:position:predicate:error:@
entriesEnumeratorWithOptions_position_predicate_error :: (IsOSLogStore osLogStore, IsOSLogPosition position, IsNSPredicate predicate, IsNSError error_) => osLogStore -> OSLogEnumeratorOptions -> position -> predicate -> error_ -> IO (Id OSLogEnumerator)
entriesEnumeratorWithOptions_position_predicate_error osLogStore  options position predicate error_ =
withObjCPtr position $ \raw_position ->
  withObjCPtr predicate $ \raw_predicate ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg osLogStore (mkSelector "entriesEnumeratorWithOptions:position:predicate:error:") (retPtr retVoid) [argCULong (coerce options), argPtr (castPtr raw_position :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | entriesEnumeratorAndReturnError
--
-- Return an OSLogEnumerator object with default options for viewing the entries; all are viewed, from earliest to latest.
--
-- @error@ — If the enumerator cannot be set up, return nil and set this to a pointer to an error object that describes the reason.
--
-- ObjC selector: @- entriesEnumeratorAndReturnError:@
entriesEnumeratorAndReturnError :: (IsOSLogStore osLogStore, IsNSError error_) => osLogStore -> error_ -> IO (Id OSLogEnumerator)
entriesEnumeratorAndReturnError osLogStore  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg osLogStore (mkSelector "entriesEnumeratorAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | positionWithDate
--
-- Return a position representing the time specified.
--
-- @date@ — The date to look for.
--
-- If there are multiple occurences of the same time --- if, for example, there was a time change during the range of entries --- the earliest occurrence is used.
--
-- ObjC selector: @- positionWithDate:@
positionWithDate :: (IsOSLogStore osLogStore, IsNSDate date) => osLogStore -> date -> IO (Id OSLogPosition)
positionWithDate osLogStore  date =
withObjCPtr date $ \raw_date ->
    sendMsg osLogStore (mkSelector "positionWithDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | positionWithTimeIntervalSinceEnd
--
-- Return a position representing an offset since the end of the time range that the entries span.
--
-- @seconds@ — The seconds to add to the last time point in the range of entries.
--
-- ObjC selector: @- positionWithTimeIntervalSinceEnd:@
positionWithTimeIntervalSinceEnd :: IsOSLogStore osLogStore => osLogStore -> CDouble -> IO (Id OSLogPosition)
positionWithTimeIntervalSinceEnd osLogStore  seconds =
  sendMsg osLogStore (mkSelector "positionWithTimeIntervalSinceEnd:") (retPtr retVoid) [argCDouble (fromIntegral seconds)] >>= retainedObject . castPtr

-- | positionWithTimeIntervalSinceLatestBoot
--
-- Return a position representing time since the last boot in the series of entries.
--
-- @seconds@ — The seconds to add to the boot time point in the log time range.
--
-- Negative seconds would create an ambiguous or imprecise position; this function asserts that the interval is positive.
--
-- ObjC selector: @- positionWithTimeIntervalSinceLatestBoot:@
positionWithTimeIntervalSinceLatestBoot :: IsOSLogStore osLogStore => osLogStore -> CDouble -> IO (Id OSLogPosition)
positionWithTimeIntervalSinceLatestBoot osLogStore  seconds =
  sendMsg osLogStore (mkSelector "positionWithTimeIntervalSinceLatestBoot:") (retPtr retVoid) [argCDouble (fromIntegral seconds)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localStoreAndReturnError:@
localStoreAndReturnErrorSelector :: Selector
localStoreAndReturnErrorSelector = mkSelector "localStoreAndReturnError:"

-- | @Selector@ for @storeWithScope:error:@
storeWithScope_errorSelector :: Selector
storeWithScope_errorSelector = mkSelector "storeWithScope:error:"

-- | @Selector@ for @storeWithURL:error:@
storeWithURL_errorSelector :: Selector
storeWithURL_errorSelector = mkSelector "storeWithURL:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @entriesEnumeratorWithOptions:position:predicate:error:@
entriesEnumeratorWithOptions_position_predicate_errorSelector :: Selector
entriesEnumeratorWithOptions_position_predicate_errorSelector = mkSelector "entriesEnumeratorWithOptions:position:predicate:error:"

-- | @Selector@ for @entriesEnumeratorAndReturnError:@
entriesEnumeratorAndReturnErrorSelector :: Selector
entriesEnumeratorAndReturnErrorSelector = mkSelector "entriesEnumeratorAndReturnError:"

-- | @Selector@ for @positionWithDate:@
positionWithDateSelector :: Selector
positionWithDateSelector = mkSelector "positionWithDate:"

-- | @Selector@ for @positionWithTimeIntervalSinceEnd:@
positionWithTimeIntervalSinceEndSelector :: Selector
positionWithTimeIntervalSinceEndSelector = mkSelector "positionWithTimeIntervalSinceEnd:"

-- | @Selector@ for @positionWithTimeIntervalSinceLatestBoot:@
positionWithTimeIntervalSinceLatestBootSelector :: Selector
positionWithTimeIntervalSinceLatestBootSelector = mkSelector "positionWithTimeIntervalSinceLatestBoot:"

