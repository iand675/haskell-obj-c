{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents single interface-bytes group of ATR.
--
-- Generated bindings for @TKSmartCardATRInterfaceGroup@.
module ObjC.CryptoTokenKit.TKSmartCardATRInterfaceGroup
  ( TKSmartCardATRInterfaceGroup
  , IsTKSmartCardATRInterfaceGroup(..)
  , ta
  , tb
  , tc
  , protocol
  , taSelector
  , tbSelector
  , tcSelector
  , protocolSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | TA interface byte of ATR group, or nil if TA is not present.
--
-- ObjC selector: @- TA@
ta :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
ta tkSmartCardATRInterfaceGroup  =
  sendMsg tkSmartCardATRInterfaceGroup (mkSelector "TA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | TB interface byte of ATR group, or nil if TB is not present.
--
-- ObjC selector: @- TB@
tb :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
tb tkSmartCardATRInterfaceGroup  =
  sendMsg tkSmartCardATRInterfaceGroup (mkSelector "TB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | TC interface byte of ATR group, or nil if TC is not present.
--
-- ObjC selector: @- TC@
tc :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
tc tkSmartCardATRInterfaceGroup  =
  sendMsg tkSmartCardATRInterfaceGroup (mkSelector "TC") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Protocol number for this group.  First group (global) has protocol unassigned, contains nil.
--
-- ObjC selector: @- protocol@
protocol :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
protocol tkSmartCardATRInterfaceGroup  =
  sendMsg tkSmartCardATRInterfaceGroup (mkSelector "protocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @TA@
taSelector :: Selector
taSelector = mkSelector "TA"

-- | @Selector@ for @TB@
tbSelector :: Selector
tbSelector = mkSelector "TB"

-- | @Selector@ for @TC@
tcSelector :: Selector
tcSelector = mkSelector "TC"

-- | @Selector@ for @protocol@
protocolSelector :: Selector
protocolSelector = mkSelector "protocol"

