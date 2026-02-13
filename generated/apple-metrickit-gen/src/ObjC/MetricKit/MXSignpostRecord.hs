{-# LANGUAGE DataKinds #-}
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
  , beginTimeStampSelector
  , categorySelector
  , dictionaryRepresentationSelector
  , durationSelector
  , endTimeStampSelector
  , isIntervalSelector
  , jsonRepresentationSelector
  , nameSelector
  , subsystemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
jsonRepresentation mxSignpostRecord =
  sendMessage mxSignpostRecord jsonRepresentationSelector

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this SignpostRecord.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSDictionary)
dictionaryRepresentation mxSignpostRecord =
  sendMessage mxSignpostRecord dictionaryRepresentationSelector

-- | subsystem
--
-- An NSString representation of the subsystem of the signpost instance.
--
-- ObjC selector: @- subsystem@
subsystem :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSString)
subsystem mxSignpostRecord =
  sendMessage mxSignpostRecord subsystemSelector

-- | category
--
-- An NSString representation of the category of the signpost instance.
--
-- ObjC selector: @- category@
category :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSString)
category mxSignpostRecord =
  sendMessage mxSignpostRecord categorySelector

-- | name
--
-- An NSString representation of the name of the signpost instance.
--
-- ObjC selector: @- name@
name :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSString)
name mxSignpostRecord =
  sendMessage mxSignpostRecord nameSelector

-- | beginTimeStamp
--
-- An NSDate representation of the begin time stamp of the signpost instance.
--
-- ObjC selector: @- beginTimeStamp@
beginTimeStamp :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSDate)
beginTimeStamp mxSignpostRecord =
  sendMessage mxSignpostRecord beginTimeStampSelector

-- | endTimeStamp
--
-- An NSDate representation of the end time stamp of the signpost instances which are intervals and will be nil for signpost events.
--
-- ObjC selector: @- endTimeStamp@
endTimeStamp :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSDate)
endTimeStamp mxSignpostRecord =
  sendMessage mxSignpostRecord endTimeStampSelector

-- | duration
--
-- An NSMeasurement representing the duration in milliseconds of signpost instances which are intervals and will be nil for signpost events.
--
-- ObjC selector: @- duration@
duration :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO (Id NSMeasurement)
duration mxSignpostRecord =
  sendMessage mxSignpostRecord durationSelector

-- | isInterval
--
-- A BOOL denoting whether the signpost instance is an interval or not..
--
-- ObjC selector: @- isInterval@
isInterval :: IsMXSignpostRecord mxSignpostRecord => mxSignpostRecord -> IO Bool
isInterval mxSignpostRecord =
  sendMessage mxSignpostRecord isIntervalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector '[] (Id NSData)
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @subsystem@
subsystemSelector :: Selector '[] (Id NSString)
subsystemSelector = mkSelector "subsystem"

-- | @Selector@ for @category@
categorySelector :: Selector '[] (Id NSString)
categorySelector = mkSelector "category"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @beginTimeStamp@
beginTimeStampSelector :: Selector '[] (Id NSDate)
beginTimeStampSelector = mkSelector "beginTimeStamp"

-- | @Selector@ for @endTimeStamp@
endTimeStampSelector :: Selector '[] (Id NSDate)
endTimeStampSelector = mkSelector "endTimeStamp"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSMeasurement)
durationSelector = mkSelector "duration"

-- | @Selector@ for @isInterval@
isIntervalSelector :: Selector '[] Bool
isIntervalSelector = mkSelector "isInterval"

