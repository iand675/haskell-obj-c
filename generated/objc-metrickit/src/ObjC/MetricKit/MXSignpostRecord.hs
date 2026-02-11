{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXSignpostRecord
--
-- A class that represents a record of signpost instance.
--
-- Signpost instances are either Signpost intervals or events and MXSignpostRecord captures information reagarding such signpost instances
--
-- Generated bindings for @MXSignpostRecord@.
module ObjC.MetricKit.MXSignpostRecord
  ( MXSignpostRecord
  , IsMXSignpostRecord(..)
  , jsonRepresentation
  , dictionaryRepresentation
  , subsystem
  , category
  , name
  , beginTimeStamp
  , endTimeStamp
  , duration
  , isInterval
  , jsonRepresentationSelector
  , dictionaryRepresentationSelector
  , subsystemSelector
  , categorySelector
  , nameSelector
  , beginTimeStampSelector
  , endTimeStampSelector
  , durationSelector
  , isIntervalSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | JSONRepresentation
--
-- Convenience method to return a JSON representation of this SignpostRecord.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSData)
jsonRepresentation mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this SignpostRecord.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSDictionary)
dictionaryRepresentation mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "dictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | subsystem
--
-- An NSString representation of the subsystem of the signpost instance.
--
-- ObjC selector: @- subsystem@
subsystem :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSString)
subsystem mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "subsystem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | category
--
-- An NSString representation of the category of the signpost instance.
--
-- ObjC selector: @- category@
category :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSString)
category mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "category") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- An NSString representation of the name of the signpost instance.
--
-- ObjC selector: @- name@
name :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSString)
name mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | beginTimeStamp
--
-- An NSDate representation of the begin time stamp of the signpost instance.
--
-- ObjC selector: @- beginTimeStamp@
beginTimeStamp :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSDate)
beginTimeStamp mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "beginTimeStamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | endTimeStamp
--
-- An NSDate representation of the end time stamp of the signpost instances which are intervals and will be nil for signpost events.
--
-- ObjC selector: @- endTimeStamp@
endTimeStamp :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSDate)
endTimeStamp mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "endTimeStamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | duration
--
-- An NSMeasurement representing the duration in milliseconds of signpost instances which are intervals and will be nil for signpost events.
--
-- ObjC selector: @- duration@
duration :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSMeasurement)
duration mxSignpostRecord  =
  sendMsg mxSignpostRecord (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isInterval
--
-- A BOOL denoting whether the signpost instance is an interval or not..
--
-- ObjC selector: @- isInterval@
isInterval :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO Bool
isInterval mxSignpostRecord  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mxSignpostRecord (mkSelector "isInterval") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @subsystem@
subsystemSelector :: Selector
subsystemSelector = mkSelector "subsystem"

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @beginTimeStamp@
beginTimeStampSelector :: Selector
beginTimeStampSelector = mkSelector "beginTimeStamp"

-- | @Selector@ for @endTimeStamp@
endTimeStampSelector :: Selector
endTimeStampSelector = mkSelector "endTimeStamp"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @isInterval@
isIntervalSelector :: Selector
isIntervalSelector = mkSelector "isInterval"

