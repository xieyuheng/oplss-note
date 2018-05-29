module Day3Door

data DoorState = DoorOpen | DoorClosed | DoorLocked

data IsClosed : DoorState -> Type where
  ClosedDoor : IsClosed DoorClosed
  LockedDoor : IsClosed DoorLocked

data DoorCmd : Type -> DoorState -> DoorState -> Type where
  Lock
    : DoorCmd () DoorClosed DoorLocked
  Unlock
    : DoorCmd () DoorLocked DoorClosed
  Open
    : DoorCmd () DoorClosed DoorOpen
  Close
    : DoorCmd () DoorOpen DoorClosed
  RingBell
    : {auto prf : IsClosed state} ->
      DoorCmd () state state
      -- `auto` means
      --   solve implicit argument also by proof search
      --   other than unification

  Pure
    : ty -> DoorCmd ty state state
  (>>=)
    : DoorCmd a state1 state2 ->
      (a -> DoorCmd b state2 state3) ->
      DoorCmd b state1 state3

doorProg : DoorCmd () DoorLocked DoorLocked
doorProg = do
  RingBell
  Unlock
  RingBell
  Open
  Close
  Lock
