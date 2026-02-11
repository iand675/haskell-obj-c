{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitInformationStorage@.
module ObjC.Foundation.NSUnitInformationStorage
  ( NSUnitInformationStorage
  , IsNSUnitInformationStorage(..)
  , bytes
  , bits
  , nibbles
  , yottabytes
  , zettabytes
  , exabytes
  , petabytes
  , terabytes
  , gigabytes
  , megabytes
  , kilobytes
  , yottabits
  , zettabits
  , exabits
  , petabits
  , terabits
  , gigabits
  , megabits
  , kilobits
  , yobibytes
  , zebibytes
  , exbibytes
  , pebibytes
  , tebibytes
  , gibibytes
  , mebibytes
  , kibibytes
  , yobibits
  , zebibits
  , exbibits
  , pebibits
  , tebibits
  , gibibits
  , mebibits
  , kibibits
  , bytesSelector
  , bitsSelector
  , nibblesSelector
  , yottabytesSelector
  , zettabytesSelector
  , exabytesSelector
  , petabytesSelector
  , terabytesSelector
  , gigabytesSelector
  , megabytesSelector
  , kilobytesSelector
  , yottabitsSelector
  , zettabitsSelector
  , exabitsSelector
  , petabitsSelector
  , terabitsSelector
  , gigabitsSelector
  , megabitsSelector
  , kilobitsSelector
  , yobibytesSelector
  , zebibytesSelector
  , exbibytesSelector
  , pebibytesSelector
  , tebibytesSelector
  , gibibytesSelector
  , mebibytesSelector
  , kibibytesSelector
  , yobibitsSelector
  , zebibitsSelector
  , exbibitsSelector
  , pebibitsSelector
  , tebibitsSelector
  , gibibitsSelector
  , mebibitsSelector
  , kibibitsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ bytes@
bytes :: IO (Id NSUnitInformationStorage)
bytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "bytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ bits@
bits :: IO (Id NSUnitInformationStorage)
bits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "bits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nibbles@
nibbles :: IO (Id NSUnitInformationStorage)
nibbles  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "nibbles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ yottabytes@
yottabytes :: IO (Id NSUnitInformationStorage)
yottabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "yottabytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ zettabytes@
zettabytes :: IO (Id NSUnitInformationStorage)
zettabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "zettabytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ exabytes@
exabytes :: IO (Id NSUnitInformationStorage)
exabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "exabytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ petabytes@
petabytes :: IO (Id NSUnitInformationStorage)
petabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "petabytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ terabytes@
terabytes :: IO (Id NSUnitInformationStorage)
terabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "terabytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gigabytes@
gigabytes :: IO (Id NSUnitInformationStorage)
gigabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "gigabytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ megabytes@
megabytes :: IO (Id NSUnitInformationStorage)
megabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "megabytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilobytes@
kilobytes :: IO (Id NSUnitInformationStorage)
kilobytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "kilobytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ yottabits@
yottabits :: IO (Id NSUnitInformationStorage)
yottabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "yottabits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ zettabits@
zettabits :: IO (Id NSUnitInformationStorage)
zettabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "zettabits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ exabits@
exabits :: IO (Id NSUnitInformationStorage)
exabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "exabits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ petabits@
petabits :: IO (Id NSUnitInformationStorage)
petabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "petabits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ terabits@
terabits :: IO (Id NSUnitInformationStorage)
terabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "terabits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gigabits@
gigabits :: IO (Id NSUnitInformationStorage)
gigabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "gigabits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ megabits@
megabits :: IO (Id NSUnitInformationStorage)
megabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "megabits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilobits@
kilobits :: IO (Id NSUnitInformationStorage)
kilobits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "kilobits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ yobibytes@
yobibytes :: IO (Id NSUnitInformationStorage)
yobibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "yobibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ zebibytes@
zebibytes :: IO (Id NSUnitInformationStorage)
zebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "zebibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ exbibytes@
exbibytes :: IO (Id NSUnitInformationStorage)
exbibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "exbibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pebibytes@
pebibytes :: IO (Id NSUnitInformationStorage)
pebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "pebibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ tebibytes@
tebibytes :: IO (Id NSUnitInformationStorage)
tebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "tebibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gibibytes@
gibibytes :: IO (Id NSUnitInformationStorage)
gibibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "gibibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mebibytes@
mebibytes :: IO (Id NSUnitInformationStorage)
mebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "mebibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kibibytes@
kibibytes :: IO (Id NSUnitInformationStorage)
kibibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "kibibytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ yobibits@
yobibits :: IO (Id NSUnitInformationStorage)
yobibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "yobibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ zebibits@
zebibits :: IO (Id NSUnitInformationStorage)
zebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "zebibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ exbibits@
exbibits :: IO (Id NSUnitInformationStorage)
exbibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "exbibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pebibits@
pebibits :: IO (Id NSUnitInformationStorage)
pebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "pebibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ tebibits@
tebibits :: IO (Id NSUnitInformationStorage)
tebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "tebibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gibibits@
gibibits :: IO (Id NSUnitInformationStorage)
gibibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "gibibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mebibits@
mebibits :: IO (Id NSUnitInformationStorage)
mebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "mebibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kibibits@
kibibits :: IO (Id NSUnitInformationStorage)
kibibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMsg cls' (mkSelector "kibibits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bytes@
bytesSelector :: Selector
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @bits@
bitsSelector :: Selector
bitsSelector = mkSelector "bits"

-- | @Selector@ for @nibbles@
nibblesSelector :: Selector
nibblesSelector = mkSelector "nibbles"

-- | @Selector@ for @yottabytes@
yottabytesSelector :: Selector
yottabytesSelector = mkSelector "yottabytes"

-- | @Selector@ for @zettabytes@
zettabytesSelector :: Selector
zettabytesSelector = mkSelector "zettabytes"

-- | @Selector@ for @exabytes@
exabytesSelector :: Selector
exabytesSelector = mkSelector "exabytes"

-- | @Selector@ for @petabytes@
petabytesSelector :: Selector
petabytesSelector = mkSelector "petabytes"

-- | @Selector@ for @terabytes@
terabytesSelector :: Selector
terabytesSelector = mkSelector "terabytes"

-- | @Selector@ for @gigabytes@
gigabytesSelector :: Selector
gigabytesSelector = mkSelector "gigabytes"

-- | @Selector@ for @megabytes@
megabytesSelector :: Selector
megabytesSelector = mkSelector "megabytes"

-- | @Selector@ for @kilobytes@
kilobytesSelector :: Selector
kilobytesSelector = mkSelector "kilobytes"

-- | @Selector@ for @yottabits@
yottabitsSelector :: Selector
yottabitsSelector = mkSelector "yottabits"

-- | @Selector@ for @zettabits@
zettabitsSelector :: Selector
zettabitsSelector = mkSelector "zettabits"

-- | @Selector@ for @exabits@
exabitsSelector :: Selector
exabitsSelector = mkSelector "exabits"

-- | @Selector@ for @petabits@
petabitsSelector :: Selector
petabitsSelector = mkSelector "petabits"

-- | @Selector@ for @terabits@
terabitsSelector :: Selector
terabitsSelector = mkSelector "terabits"

-- | @Selector@ for @gigabits@
gigabitsSelector :: Selector
gigabitsSelector = mkSelector "gigabits"

-- | @Selector@ for @megabits@
megabitsSelector :: Selector
megabitsSelector = mkSelector "megabits"

-- | @Selector@ for @kilobits@
kilobitsSelector :: Selector
kilobitsSelector = mkSelector "kilobits"

-- | @Selector@ for @yobibytes@
yobibytesSelector :: Selector
yobibytesSelector = mkSelector "yobibytes"

-- | @Selector@ for @zebibytes@
zebibytesSelector :: Selector
zebibytesSelector = mkSelector "zebibytes"

-- | @Selector@ for @exbibytes@
exbibytesSelector :: Selector
exbibytesSelector = mkSelector "exbibytes"

-- | @Selector@ for @pebibytes@
pebibytesSelector :: Selector
pebibytesSelector = mkSelector "pebibytes"

-- | @Selector@ for @tebibytes@
tebibytesSelector :: Selector
tebibytesSelector = mkSelector "tebibytes"

-- | @Selector@ for @gibibytes@
gibibytesSelector :: Selector
gibibytesSelector = mkSelector "gibibytes"

-- | @Selector@ for @mebibytes@
mebibytesSelector :: Selector
mebibytesSelector = mkSelector "mebibytes"

-- | @Selector@ for @kibibytes@
kibibytesSelector :: Selector
kibibytesSelector = mkSelector "kibibytes"

-- | @Selector@ for @yobibits@
yobibitsSelector :: Selector
yobibitsSelector = mkSelector "yobibits"

-- | @Selector@ for @zebibits@
zebibitsSelector :: Selector
zebibitsSelector = mkSelector "zebibits"

-- | @Selector@ for @exbibits@
exbibitsSelector :: Selector
exbibitsSelector = mkSelector "exbibits"

-- | @Selector@ for @pebibits@
pebibitsSelector :: Selector
pebibitsSelector = mkSelector "pebibits"

-- | @Selector@ for @tebibits@
tebibitsSelector :: Selector
tebibitsSelector = mkSelector "tebibits"

-- | @Selector@ for @gibibits@
gibibitsSelector :: Selector
gibibitsSelector = mkSelector "gibibits"

-- | @Selector@ for @mebibits@
mebibitsSelector :: Selector
mebibitsSelector = mkSelector "mebibits"

-- | @Selector@ for @kibibits@
kibibitsSelector :: Selector
kibibitsSelector = mkSelector "kibibits"

