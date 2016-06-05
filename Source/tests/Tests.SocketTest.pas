unit Tests.SocketTest;

interface

{$WARNINGS OFF}
{$HINTS OFF}

type
  SocketTest = class sealed
  private
    class procedure SocketConnect(Sender: TObject);
    class procedure SocketDisconnect(Sender: TObject);
  public
    class procedure DoTest;
  end;

implementation

uses
  {$IFDEF VER210}
  Windows,
  Math, Generics.Collections,
  {$ELSE}
  Winapi.Windows,
  System.Math, System.Generics.Collections,
  {$ENDIF}
  Utils.Messages, Utils.L2Buffer,
  Net.L2CustomClient, Net.L2Packet;

type
  TLineage2DebugAsyncSocket = class(TLineage2CustomClient)
  private
    // debug read

    ProceededPacketCount: UInt64;
    DebugPacketQueue: TList<TLineage2Buffer>;
    procedure DebugProcessRead;
    procedure AddPacketToDebugQueue(Packet: PLineage2Buffer);

    // debug write

    procedure DebugProcessWrite;
  public
    constructor Create;
  end;

class procedure SocketTest.DoTest;
var
  Socket: TLineage2DebugAsyncSocket;
begin
  Socket:= TLineage2DebugAsyncSocket.Create;

  // Socket.DebugProcessRead;

  Socket.DebugProcessWrite;

  DispatchMessages;
end;

class procedure SocketTest.SocketConnect(Sender: TObject);
begin
  WriteLn('connected');
end;

class procedure SocketTest.SocketDisconnect(Sender: TObject);
begin
  WriteLn('disconnected');
end;

{ TLineage2DebugAsyncSocket }

constructor TLineage2DebugAsyncSocket.Create;
begin
  inherited;
  Self.DebugPacketQueue:= TList<TLineage2Buffer>.Create;
end;

{ debug read }

procedure TLineage2DebugAsyncSocket.DebugProcessRead;
var
  StartTime: Cardinal;

  procedure RerandomizeIfNeed;
  var
    Now, Ellapsed: Cardinal;
  begin
    Now:= GetTickCount;
    Ellapsed:= Now - StartTime;
    if Ellapsed >= 15 * 1000 then
    begin
      Randomize;
      StartTime:= Now;
    end;
  end;

var
  SinglePacket: TLineage2Buffer;

  procedure CreateRandomPacket;
  var
    PacketLength: Word;
  begin
    PacketLength:= Random(MaxFullPacketLen + 1);

    SinglePacket.Reset;

    SinglePacket.WriteW(PacketLength);

    while SinglePacket.ReadRemaining < PacketLength do
      SinglePacket.WriteB(Random(High(Byte) - Low(Byte) + 1));

    Assert(SinglePacket.ReadRemaining = Max(PacketLenSize, PacketLength));
  end;

var
  Partition: TLineage2Buffer;
  PartitionSize, ByteSizeToFill, AvailableByteSize, BytesToRead: Integer;
begin
  Partition:= TLineage2Buffer.Create;
  SinglePacket:= TLineage2Buffer.Create;

  StartTime:= GetTickCount;

  Randomize;

  ProceededPacketCount:= 0;

  repeat
    Partition.Reset;

    PartitionSize:= 1 + Random(MaxFullPacketLen);
    while True do
    begin
      if SinglePacket.ReadRemaining = 0 then
      begin
        CreateRandomPacket;
        // if packet have any actual data - put it on waiting queue
        if SinglePacket.ReadRemaining > PacketLenSize then
          AddPacketToDebugQueue(@SinglePacket);
      end;

      ByteSizeToFill:= PartitionSize - Partition.ReadRemaining;

      if ByteSizeToFill = 0 then
        Break;
      Assert(ByteSizeToFill > 0);

      AvailableByteSize:= SinglePacket.ReadRemaining;
      Assert(AvailableByteSize > 0);

      BytesToRead:= Min(ByteSizeToFill, AvailableByteSize);

      Partition.ReadFrom(@SinglePacket, BytesToRead);
    end;

    // here we have filled

    Self.ProcessRead(@Partition);

    Assert(Partition.IsEmpty);

    RerandomizeIfNeed;
  until False;
end;

procedure TLineage2DebugAsyncSocket.AddPacketToDebugQueue(Packet: PLineage2Buffer);
begin
  DebugPacketQueue.Add(Packet.Copy);
end;
      {
procedure TLineage2DebugAsyncSocket.ProcessPacket(Data: PLineage2Buffer);
var
  PacketTemplate: TLineage2Buffer;
  PacketTemplateLen: Word;
begin
  PacketTemplate:= DebugPacketQueue.First;
  DebugPacketQueue.Delete(0);

  PacketTemplate.TryPeekPacketLength(PacketTemplateLen);
  Assert(PacketTemplateLen = Data.ReadRemaining + PacketLenSize);
  PacketTemplate.SeekReadPosition(PacketLenSize);

  Assert(PacketTemplate.CompareTo(Data));

  Inc(ProceededPacketCount);
end;
}

{ debug write }

type
  TLineage2DebugPacket = class(TLineage2CustomClientPacket)
  protected
    procedure Write; override;
  private
    Bytes: TArray<Byte>;
  public
    constructor Create;
  end;

procedure TLineage2DebugAsyncSocket.DebugProcessWrite;
var
  Buffer: TLineage2Buffer;
begin
  Buffer:= TLineage2Buffer.Create;
  repeat
    Buffer.Reset;

    Self.SendPacket(TLineage2DebugPacket.Create);

    Self.ProcessWrite(@Buffer);
    Self.ProcessWriteDoCleanup;

    Buffer.PrintDump;

    Break;
  until False;
  ReadLn;
end;

{ TLineage2DebugPacket }

constructor TLineage2DebugPacket.Create;
var
  Index: Integer;
begin
  inherited;
  SetLength(Bytes, 1 + Random(MaxPacketDataSize));
  for Index := 0 to Length(Bytes) - 1 do
    Bytes[Index]:= Random(High(Byte) + 1);
end;

procedure TLineage2DebugPacket.Write;
var
  D: Double;
begin
  D:= 1.9;
  WriteMask('chdsbfq', [UInt64(1), 2, 3, ShortString('test'), TArray<Byte>.Create(9, 7, 5), D, Byte(4)]);
  // WriteBytes(Bytes);
end;

end.
