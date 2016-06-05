unit Utils.L2Buffer;

interface

uses
  {$IFDEF VER210}
  SysUtils, Math,
  {$ELSE}
  System.SysUtils, System.Math,
  {$ENDIF}
  Utils;

const
  PacketLenSize = SizeOf(Word);
  MaxPacketDataSize = High(Word) - PacketLenSize;
  // len + data[len - SizeOf(len)]
  MaxFullPacketLen = PacketLenSize + MaxPacketDataSize;

type
  TLineage2BufferData = array [0..MaxFullPacketLen - 1] of Byte;

  PLineage2Buffer = ^TLineage2Buffer;
  TLineage2Buffer = record
  strict private
    Data: TLineage2BufferData;
    Position, DataLen: Integer;
  private
  public
    class function Create : TLineage2Buffer; static;
    class function Allocate : PLineage2Buffer; static;
    procedure Reset; {$IFNDEF DEBUG} inline; {$ENDIF}

    function TryReadFrom(const Src; Size: Integer) : Boolean; overload;
    function TryReadFrom(Src: PLineage2Buffer; Size: Integer; Peek: Boolean = False) : Boolean; overload;

    procedure ReadFrom(Src: PLineage2Buffer; Size: Integer; Peek: Boolean = False); overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure ReadFrom(Src: PLineage2Buffer; Peek: Boolean = False); overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure ReadFrom(const Src; Size: Integer); overload; {$IFNDEF DEBUG} inline; {$ENDIF}

    function TryReadTo(var Dst; Size: Integer; Peek: Boolean = False) : Boolean;

    procedure ReadTo(var Dst; Size: Integer); {$IFNDEF DEBUG} inline; {$ENDIF}

    function TryFillChar(Count: Integer; Value: Byte) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}

    function ReadRemaining : Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
    function WriteRemaining : Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
    function IsEmpty : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}

    function GetWritePtr : PByte; {$IFNDEF DEBUG} inline; {$ENDIF}
    function GetReadPtr : PByte; {$IFNDEF DEBUG} inline; {$ENDIF}

    procedure SeekReadPosition(Size: Integer); {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure SeekWritePosition(Size: Integer); {$IFNDEF DEBUG} inline; {$ENDIF}

    function TrySeekReadPosition(Size: Integer) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
    function TrySeekWritePosition(Size: Integer) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}

    procedure SetReadPosition(Position: Integer); {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure SetWritePosition(Position: Integer); {$IFNDEF DEBUG} inline; {$ENDIF}

    function TrySetReadPosition(Position: Integer) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
    function TrySetWritePosition(Position: Integer) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}

    function CompareToData(Data: PByte; Len: Integer) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
    function CompareTo(Other: PLineage2Buffer) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}

    procedure WriteB(B: Byte); {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure WriteW(W: Word); {$IFNDEF DEBUG} inline; {$ENDIF}

    function ReadB : Byte; {$IFNDEF DEBUG} inline; {$ENDIF}

    function Copy : TLineage2Buffer; {$IFNDEF DEBUG} inline; {$ENDIF}

    function TryPeekPacketLength(out PacketLength: Word) : Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}

    function ReadUnicodeNullTerminatedString(out HaveFoundNullChar: Boolean) : UnicodeString;

    procedure PrintDump;

    property ReadPosition : Integer read Position;
    property WritePosition : Integer read DataLen;
  end;

implementation

{ TLineage2Buffer }

class function TLineage2Buffer.Create: TLineage2Buffer;
begin
  Result.Reset;
end;

class function TLineage2Buffer.Allocate: PLineage2Buffer;
begin
  GetMem(Result, SizeOf(Result^));
  Result.Reset;
end;

procedure TLineage2Buffer.Reset;
begin
  Self.Position:= 0;
  Self.DataLen:= 0;
end;

function TLineage2Buffer.IsEmpty: Boolean;
begin
  Result:= ReadRemaining = 0;
end;

function TLineage2Buffer.GetReadPtr: PByte;
begin
  Result:= PByte(@Self.Data) + Self.Position;
end;

function TLineage2Buffer.GetWritePtr: PByte;
begin
  Result:= PByte(@Self.Data) + Self.DataLen;
end;

procedure TLineage2Buffer.SeekReadPosition(Size: Integer);
begin
  Assert(TrySeekReadPosition(Size));
end;

procedure TLineage2Buffer.SeekWritePosition(Size: Integer);
begin
  Assert(TrySeekWritePosition(Size));
end;

function TLineage2Buffer.TrySeekReadPosition(Size: Integer): Boolean;
begin
  Assert(Size >= 0);
  Result:= TrySetReadPosition(Self.Position + Size);
end;

function TLineage2Buffer.TrySeekWritePosition(Size: Integer): Boolean;
begin
  Assert(Size >= 0);
  Result:= TrySetWritePosition(Self.DataLen + Size);
end;

procedure TLineage2Buffer.SetReadPosition(Position: Integer);
begin
  Assert(TrySetReadPosition(Position));
end;

procedure TLineage2Buffer.SetWritePosition(Position: Integer);
begin
  Assert(TrySetWritePosition(Position));
end;

function TLineage2Buffer.TrySetReadPosition(Position: Integer): Boolean;
begin
  if (Position >= 0) and (Position <= Length(Self.Data)) then
  begin
    Self.Position:= Position;
    Result:= True;
  end
  else
    Result:= False;
end;

function TLineage2Buffer.TrySetWritePosition(Position: Integer): Boolean;
begin
  if (Position >= 0) and (Position <= Length(Self.Data)) then
  begin
    Self.DataLen:= Position;
    Result:= True;
  end
  else
    Result:= False;
end;

function TLineage2Buffer.CompareToData(Data: PByte; Len: Integer): Boolean;
var
  Remaining: Integer;
begin
  Remaining:= Self.ReadRemaining;
  Result:= (Remaining = Len) and CompareMem(GetReadPtr, Data, Len);
end;

function TLineage2Buffer.CompareTo(Other: PLineage2Buffer): Boolean;
begin
  Result:= CompareToData(Other.GetReadPtr, Other.ReadRemaining);
end;

function TLineage2Buffer.TryReadFrom(Src: PLineage2Buffer; Size: Integer; Peek: Boolean) : Boolean;
var
  Dst: TLineage2Buffer absolute Self;
  Remaining: Integer;
begin
  Remaining:= Dst.WriteRemaining;

  if Remaining < Size then
    Exit(False);

  if Src.TryReadTo(Dst.GetWritePtr^, Size, Peek) then
  begin
    Dst.SeekWritePosition(Size);
    Result:= True;
  end
  else
    Result:= False;
end;

function TLineage2Buffer.TryReadTo(var Dst; Size: Integer; Peek: Boolean) : Boolean;
var
  Src: TLineage2Buffer absolute Self;
  Remaining: Integer;
begin
  Remaining:= Src.ReadRemaining;

  if Remaining < Size then
    Exit(False);

  Move(Src.GetReadPtr^, Dst, Size);
  if not Peek then
    Src.SeekReadPosition(Size);

  Result:= True;
end;

function TLineage2Buffer.TryReadFrom(const Src; Size: Integer): Boolean;
var
  Dst: TLineage2Buffer absolute Self;
  Remaining: Integer;
begin
  Remaining:= Dst.WriteRemaining;

  if Remaining < Size then
    Exit(False);

  Move(Src, Dst.GetWritePtr^, Size);
  Dst.SeekWritePosition(Size);

  Result:= True;
end;

function TLineage2Buffer.TryFillChar(Count: Integer; Value: Byte): Boolean;
var
  Dst: TLineage2Buffer absolute Self;
  Remaining: Integer;
begin
  Assert(Count > 0);

  Remaining:= Dst.WriteRemaining;

  if Remaining < Count then
    Exit(False);

  FillChar(Dst.GetWritePtr^, Count, Value);
  Dst.SeekWritePosition(Count);

  Result:= True;
end;

procedure TLineage2Buffer.ReadFrom(Src: PLineage2Buffer; Size: Integer; Peek: Boolean);
begin
  Assert(TryReadFrom(Src, Size, Peek));
end;

procedure TLineage2Buffer.ReadFrom(Src: PLineage2Buffer; Peek: Boolean);
begin
  ReadFrom(Src, Src.ReadRemaining, Peek);
end;

procedure TLineage2Buffer.ReadFrom(const Src; Size: Integer);
begin
  Assert(TryReadFrom(Src, Size));
end;

procedure TLineage2Buffer.ReadTo(var Dst; Size: Integer);
begin
  Assert(TryReadTo(Dst, Size));
end;

function TLineage2Buffer.ReadRemaining: Integer;
begin
  Result:= Self.DataLen - Self.Position;
end;

function TLineage2Buffer.WriteRemaining: Integer;
begin
  Result:= Length(Self.Data) - Self.DataLen;
end;

function TLineage2Buffer.TryPeekPacketLength(out PacketLength: Word): Boolean;
const
  PacketMinLength = 2;
begin
  Assert(SizeOf(PacketLength) = PacketLenSize);

  if TryReadTo(PacketLength, SizeOf(PacketLength), True) then
  begin
    PacketLength:= Max(PacketMinLength, PacketLength);
    Result:= True;
  end
  else
    Result:= False;
end;

function TLineage2Buffer.ReadUnicodeNullTerminatedString(out HaveFoundNullChar: Boolean) : UnicodeString;
var
  SrcPtr: PByte;
  SrcLen, CharsCount, CharLen, ByteSize, Counter: Integer;
  CharPtr: PWideChar;
  C: WideChar;
begin
  SrcPtr:= Self.GetReadPtr;
  SrcLen:= Self.ReadRemaining;

  CharsCount:= SrcLen div SizeOf(Result[1]);

  CharPtr:= PWideChar(SrcPtr);

  CharLen:= 0;
  ByteSize:= 0;

  HaveFoundNullChar:= False;

  for Counter := CharsCount downto 1 do
  begin
    C:= CharPtr^;

    Inc(ByteSize, SizeOf(C));

    if C = #0 then
    begin
      HaveFoundNullChar:= True;
      Break;
    end;

    Inc(CharLen);

    Inc(CharPtr);
  end;

  SetLength(Result, CharLen);
  if CharLen > 0 then
    Move(SrcPtr^, Result[1], CharLen * SizeOf(Result[1]));

  Self.SeekReadPosition(ByteSize);
end;

function TLineage2Buffer.Copy: TLineage2Buffer;
begin
  Result:= Self;
end;

procedure TLineage2Buffer.WriteB(B: Byte);
begin
  ReadFrom(B, SizeOf(B));
end;

procedure TLineage2Buffer.WriteW(W: Word);
begin
  ReadFrom(W, SizeOf(W));
end;

function TLineage2Buffer.ReadB: Byte;
begin
  ReadTo(Result, SizeOf(Result));
end;

procedure TLineage2Buffer.PrintDump;
begin
  WriteLn(printData(Self.GetReadPtr, Self.ReadRemaining));
end;

end.
