unit Net.L2Packet;

interface

uses
  {$IFDEF VER210}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  Utils.L2Buffer;

type
  ILineage2CustomClientPacket = interface
    ['{63674A1A-F8C5-4855-A7FE-005D5C392E6E}']
    function WriteTo(Data: PLineage2Buffer) : Boolean;
    function DoPostProc(Data: PLineage2Buffer) : Boolean;
  end;

  TLineage2CustomClientPacket = class(TInterfacedObject, ILineage2CustomClientPacket)
  private
    DstBuffer: PLineage2Buffer;
    IsOverflowOccurred, IsPacketIdSet: Boolean;
    PacketId: Byte;

    function IsWriteActionValid : Boolean; inline;
    procedure InvalidateWriteActions; inline;

    procedure WriteOverflowSafe(const Data; Size: Integer);
    procedure FillCharOverflowSafe(Count: Integer; Value: Byte);
  protected
    // strict rule for these two methods - they should be idempotent (like GET in HTTP)
    procedure PostProcess(Data: PLineage2Buffer); virtual;
    procedure Write; virtual; abstract;

    // utils
    procedure SetPacketId(PacketId: Byte); inline;
    procedure WriteC(B: Byte); inline;
    procedure WriteH(W: Word); inline;
    procedure WriteD(D: Cardinal); overload; inline;
    procedure WriteD(I: Integer); overload; inline;
    procedure WriteF(D: Double); inline;
    procedure WriteQ(Q: Int64); overload; inline;
    procedure WriteQ(Q: UInt64); overload; inline;
    procedure WriteB(const Bytes: array of Byte); overload;
    procedure WriteB(const Bytes: TArray<Byte>); overload;
    procedure WriteS(const S: UnicodeString); inline;
    procedure WriteZ(Count: Integer); inline;
    procedure WriteMask(const Format: String; const Args: array of const);
  public
    function WriteTo(Dst: PLineage2Buffer) : Boolean;
    function DoPostProc(Data: PLineage2Buffer) : Boolean;
  end;

  TLineage2ServerPacketClass = class of TLineage2CustomServerPacket;

  TLineage2CustomServerPacket = class
  strict private
    SrcBuffer: PLineage2Buffer;
    IsOverrunOccurred: Boolean;

    procedure ReadOverrunSafe(var Dst; Size: Integer);
  strict protected
    Owner: TObject;
  protected
    procedure Read; virtual; abstract;
    procedure Run; virtual; abstract;

    function ReadC : Byte; inline;
    function ReadH : Word; inline;
    function ReadD : Integer; inline;
    function ReadF : Double; inline;
    function ReadQ : Int64; inline;
    function ReadB(Size: Integer) : TArray<Byte>;
    function ReadS : UnicodeString; inline;
    procedure ReadMask(const Format: String; const Outputs: array of Pointer); deprecated;
    procedure Skip(const Format: String); overload;
    procedure Skip(BytesCount: Integer); overload;
    function ReadBool : Boolean; inline;
  public
    procedure ReadFrom(Src: PLineage2Buffer);
    procedure RunPacket;
    constructor Create(Owner: TObject);
  end;

implementation

{ TLineage2CustomClientPacket }

function TLineage2CustomClientPacket.WriteTo(Dst: PLineage2Buffer) : Boolean;
var
  PacketStartPosition: Integer;

  procedure RewritePacketId;
  var
    PacketEndPosition: Integer;
  begin
    if not Self.IsOverflowOccurred then
    begin
      // save packet end position
      PacketEndPosition:= Self.DstBuffer.WritePosition;

      // seek to packet start
      Self.DstBuffer.SetWritePosition(PacketStartPosition);

      // rewrite packet id
      WriteC(PacketId);
      // cannot fail after rewriting
      Assert(not Self.IsOverflowOccurred);

      // rollback at the end position
      Self.DstBuffer.SetWritePosition(PacketEndPosition);
    end;
  end;

begin
  try
    // saving a dst buffer
    Self.DstBuffer:= Dst;
    // setup flags
    Self.IsOverflowOccurred:= False;
    Self.IsPacketIdSet:= False;

    PacketStartPosition:= Self.DstBuffer.WritePosition;
    // allocate packet ID
    WriteC(0);

    // writing a packet
    Write;

    Assert(Self.IsPacketIdSet, 'Packet Id should be set');

    RewritePacketId;

    Result:= not IsOverflowOccurred;
  finally
    Self.DstBuffer:= nil;
  end;
end;

function TLineage2CustomClientPacket.DoPostProc(Data: PLineage2Buffer): Boolean;
begin
  try
    Assert(not IsOverflowOccurred);

    // saving a dst buffer
    Self.DstBuffer:= Data;

    PostProcess(Data);

    Result:= not IsOverflowOccurred;
  finally
    Self.DstBuffer:= nil;
  end;
end;

procedure TLineage2CustomClientPacket.PostProcess(Data: PLineage2Buffer);
begin
end;

function TLineage2CustomClientPacket.IsWriteActionValid: Boolean;
begin
  Assert(Assigned(DstBuffer));

  Result:= not IsOverflowOccurred;
end;

procedure TLineage2CustomClientPacket.InvalidateWriteActions;
begin
  Assert(Assigned(DstBuffer));

  IsOverflowOccurred:= True;
end;

procedure TLineage2CustomClientPacket.WriteOverflowSafe(const Data; Size: Integer);
begin
  if not IsWriteActionValid then Exit;

  if not DstBuffer.TryReadFrom(Data, Size) then
    InvalidateWriteActions;
end;

procedure TLineage2CustomClientPacket.FillCharOverflowSafe(Count: Integer; Value: Byte);
begin
  if not IsWriteActionValid then Exit;

  if not DstBuffer.TryFillChar(Count, Value) then
    InvalidateWriteActions;
end;

procedure TLineage2CustomClientPacket.SetPacketId(PacketId: Byte);
begin
  Self.PacketId:= PacketId;
  Self.IsPacketIdSet:= True;
end;

procedure TLineage2CustomClientPacket.WriteC(B: Byte);
begin
  WriteOverflowSafe(B, SizeOf(B));
end;

procedure TLineage2CustomClientPacket.WriteH(W: Word);
begin
  WriteOverflowSafe(W, SizeOf(W));
end;

procedure TLineage2CustomClientPacket.WriteD(D: Cardinal);
begin
  WriteOverflowSafe(D, SizeOf(D));
end;

procedure TLineage2CustomClientPacket.WriteD(I: Integer);
begin
  WriteOverflowSafe(I, SizeOf(I));
end;

procedure TLineage2CustomClientPacket.WriteF(D: Double);
begin
  WriteOverflowSafe(D, SizeOf(D));
end;

procedure TLineage2CustomClientPacket.WriteQ(Q: Int64);
begin
  WriteOverflowSafe(Q, SizeOf(Q));
end;

procedure TLineage2CustomClientPacket.WriteQ(Q: UInt64);
begin
  WriteOverflowSafe(Q, SizeOf(Q));
end;

procedure TLineage2CustomClientPacket.WriteB(const Bytes: array of Byte);
begin
  if Length(Bytes) = 0 then Exit;
  WriteOverflowSafe(Bytes[0], Length(Bytes) * SizeOf(Bytes[0]));
end;

procedure TLineage2CustomClientPacket.WriteB(const Bytes: TArray<Byte>);
begin
  if Length(Bytes) = 0 then Exit;
  WriteOverflowSafe(Bytes[0], Length(Bytes) * SizeOf(Bytes[0]));
end;

procedure TLineage2CustomClientPacket.WriteS(const S: UnicodeString);
begin
  WriteOverflowSafe(S[1], Length(S) * SizeOf(S[1]));
  WriteZ(SizeOf(S[1])); // null terminating character
end;

procedure TLineage2CustomClientPacket.WriteZ(Count: Integer);
begin
  FillCharOverflowSafe(Count, 0);
end;

procedure TLineage2CustomClientPacket.WriteMask(const Format: String; const Args: array of const);
var
  Index: Integer;
  C: Char;
  Arg: PVarRec;

  procedure RaiseInvalidType;
  const
    TypeToStringTable : array [vtInteger..vtUnicodeString] of String =
    (
      'Integer', 'Boolean', 'Char', 'Extended', 'String', 'Pointer', 'PChar', 'Object', 'Class', 'WideChar',
      'PWideChar', 'AnsiString', 'Currency', 'Variant', 'Interface', 'WideString', 'Int64', 'UnicodeString'
    );
  var
    TypeStr: String;
  begin
    TypeStr:= TypeToStringTable[Arg.VType];
    raise EArgumentException.CreateFmt('Invalid type "%s" passed for mask char "%s"', [TypeStr, C]);
  end;

type
  PBytes = ^TBytes;

begin
  Assert(Length(Format) = Length(Args));
  for Index := 0 to Length(Format) - 1 do
  begin
    C:= Format[1 + Index];
    Arg:= @Args[Index];

    case C of
    'c': // byte
    begin
      case Arg.VType of
      vtInteger: WriteC(Cardinal(Arg.VInteger));
      vtInt64:   WriteC(UInt64(Arg.VInt64^));
      else RaiseInvalidType;
      end;
    end;
    'h': // word
    begin
      case Arg.VType of
      vtInteger: WriteH(Cardinal(Arg.VInteger));
      vtInt64:   WriteH(UInt64(Arg.VInt64^));
      else RaiseInvalidType;
      end;
    end;
    'd', 'i': // integer
    begin
      case Arg.VType of
      vtInteger: WriteD(Integer(Arg.VInteger));
      vtInt64:   WriteD(Int64(Arg.VInt64^));
      else RaiseInvalidType;
      end;
    end;
    's': // unicode string
    begin
      {$WARN IMPLICIT_STRING_CAST OFF}
      case Arg.VType of
      vtChar: WriteS(Arg.VChar);
      vtString: WriteS(Arg.VString^);
      vtPChar: WriteS(Arg.VPChar);
      vtWideChar: WriteS(Arg.VWideChar);
      vtPWideChar: WriteS(Arg.VPWideChar);
      vtAnsiString: WriteS(PAnsiString(@Arg.VAnsiString)^);
      vtWideString: WriteS(PWideString(@Arg.VWideString)^);
      vtUnicodeString: WriteS(PUnicodeString(@Arg.VUnicodeString)^);
      else RaiseInvalidType;
      end;
      {$WARN IMPLICIT_STRING_CAST ON}
    end;
    'b': // byte array
    begin
      case Arg.VType of
      vtPointer: WriteB(PBytes(@Arg.VPointer)^);
      else RaiseInvalidType;
      end;
    end;
    'f': // double
    begin
      case Arg.VType of
      vtExtended: WriteF(Arg.VExtended^);
      else RaiseInvalidType;
      end;
    end;
    'q': // Int64
    begin
      case Arg.VType of
      vtInteger: WriteQ(Integer(Arg.VInteger));
      vtInt64:   WriteQ(Int64(Arg.VInt64^));
      else RaiseInvalidType;
      end;
    end;
    else raise EArgumentException.CreateFmt('Unknown character in format string: "%s"', [C]);
    end;
  end;
end;

{ TLineage2CustomServerPacket }

constructor TLineage2CustomServerPacket.Create(Owner: TObject);
begin
  inherited Create;
  Self.Owner:= Owner;
end;

procedure TLineage2CustomServerPacket.ReadFrom(Src: PLineage2Buffer);
begin
  try
    Self.SrcBuffer:= Src;
    Self.IsOverrunOccurred:= False;

    Read;
  finally
    Self.SrcBuffer:= nil;
  end;
end;

procedure TLineage2CustomServerPacket.RunPacket;
begin
  Run;
end;

procedure TLineage2CustomServerPacket.ReadOverrunSafe(var Dst; Size: Integer);
var
  IsReadFailed: Boolean;
begin
  Assert(Assigned(Self.SrcBuffer));

  if Self.IsOverrunOccurred then
    IsReadFailed:= True
  else
  if not Self.SrcBuffer.TryReadTo(Dst, Size) then
  begin
    Self.IsOverrunOccurred:= True;
    IsReadFailed:= True;
  end
  else
    IsReadFailed:= False;

  if IsReadFailed then
    FillChar(Dst, Size, 0);
end;

function TLineage2CustomServerPacket.ReadC: Byte;
begin
  ReadOverrunSafe(Result, SizeOf(Result));
end;

function TLineage2CustomServerPacket.ReadH: Word;
begin
  ReadOverrunSafe(Result, SizeOf(Result));
end;

function TLineage2CustomServerPacket.ReadD: Integer;
begin
  ReadOverrunSafe(Result, SizeOf(Result));
end;

function TLineage2CustomServerPacket.ReadF: Double;
begin
  ReadOverrunSafe(Result, SizeOf(Result));
end;

function TLineage2CustomServerPacket.ReadQ: Int64;
begin
  ReadOverrunSafe(Result, SizeOf(Result));
end;

function TLineage2CustomServerPacket.ReadB(Size: Integer): TArray<Byte>;
begin
  SetLength(Result, Size);
  Size:= Length(Result);
  if Size > 0 then
    ReadOverrunSafe(Result[0], Size);
end;

function TLineage2CustomServerPacket.ReadS: UnicodeString;
var
  HaveFoundNullChar: Boolean;
begin
  Assert(Assigned(Self.SrcBuffer));
  if IsOverrunOccurred then Exit('');

  Result:= Self.SrcBuffer.ReadUnicodeNullTerminatedString(HaveFoundNullChar);

  if not HaveFoundNullChar then
    IsOverrunOccurred:= True;
end;

procedure TLineage2CustomServerPacket.ReadMask(const Format: String; const Outputs: array of Pointer);
var
  Index: Integer;
  C: Char;
  Output: Pointer;
begin
  for Index := 0 to Length(Format) - 1 do
  begin
    C:= Format[1 + Index];
    Output:= Outputs[Index];
    case C of
    'c': PByte(Output)^:= ReadC;
    'h': PWord(Output)^:= ReadH;
    'd': PInteger(Output)^:= ReadD;
    'f': PDouble(Output)^:= ReadF;
    'q': PInt64(Output)^:= ReadQ;
    's': PUnicodeString(Output)^:= ReadS;
    else EArgumentException.CreateFmt('Invalid char "%s" in format', [C]);
    end;
  end;
end;

procedure TLineage2CustomServerPacket.Skip(const Format: String);
var
  C: Char;
begin
  for C in Format do
    case C of
    'c': ReadC;
    'h': ReadH;
    'd': ReadD;
    'f': ReadF;
    'q': ReadQ;
    's': ReadS;
    else EArgumentException.CreateFmt('Invalid char "%s" in format', [C]);
    end;
end;

procedure TLineage2CustomServerPacket.Skip(BytesCount: Integer);
var
  Skipped: Boolean;
begin
  Assert(Assigned(Self.SrcBuffer));
  if IsOverrunOccurred then Exit;

  Skipped:= Self.SrcBuffer.TrySeekReadPosition(BytesCount);

  if not Skipped then
    IsOverrunOccurred:= True;
end;

function TLineage2CustomServerPacket.ReadBool: Boolean;
begin
  Result:= ReadC <> 0;
end;

end.
