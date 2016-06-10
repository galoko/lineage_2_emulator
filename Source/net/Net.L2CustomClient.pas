unit Net.L2CustomClient;

interface

uses
  {$IFDEF VER210}
  Windows, Messages, Winsock,
  RTLConsts, SysUtils, Classes, Math, ScktComp, Generics.Collections,
  {$ELSE}
  Winapi.Windows, Winapi.Messages, Winapi.Winsock,
  System.RTLConsts, System.SysUtils, System.Classes, System.Math, System.Win.ScktComp, System.Generics.Collections,
  {$ENDIF}
  Net.L2Packet,
  Utils.L2Buffer;

type
  TLineage2CustomClient = class
  public
    const
      DEFAULT_LOGIN_PORT = 2106;
      DEFAULT_GAME_PORT  = 7777;
  private
    const
      WM_ASYNC_DISCONNECT = WM_USER + 7;
      WM_PUSH_WRITE = WM_USER + 8;
    var
      SocketHandle: TSocket;
      WindowHandle: HWND;

      Connecting, NeedRead, WriteBlocked, Reading, Writing, IsWritePushed: Boolean;

      PacketReassembleBuffer: TLineage2Buffer;

      PacketQueue: TList<ILineage2CustomClientPacket>;
      WritedPacketsCount: Integer;

    procedure WndProc(var Message: TMessage);

    procedure CloseSocketHandle(SetupAsync: Boolean = True);
    procedure CloseWindowHandle;

    procedure CMSocketMessage(var Message: TCMSocketMessage); message CM_SOCKETMESSAGE;
    procedure WMAsyncDisconnect(var Message: TMessage); message WM_ASYNC_DISCONNECT;
    procedure WMPushWrite(var Message: TMessage); message WM_PUSH_WRITE;

    procedure EventConnect;
    procedure EventDisconnect;
    procedure EventRead;
    procedure EventWrite;

    procedure SetupDelayedDisconnectEvent;

    procedure Setup;

    procedure PushWrite;

    function ProcessWritePacket(Data: PLineage2Buffer; const Packet: ILineage2CustomClientPacket) : Boolean;

  // for debug, should be private
  protected 
    procedure DoNotify(Notify: TNotifyEvent); inline;

    procedure ProcessWrite(Data: PLineage2Buffer);
    procedure ProcessWriteDoCleanup;
    procedure ProcessRead(Data: PLineage2Buffer);
  protected
    procedure Connected; virtual; abstract;
    procedure DecryptPacket(Data: PLineage2Buffer); virtual; abstract;
    procedure EncryptPacket(Data: PLineage2Buffer); virtual; abstract;
    function GetPacketClass(PacketID: Byte) : TLineage2ServerPacketClass; virtual; abstract;
  public
    Address: Cardinal;
    Port: Word;

    OnConnect, OnDisconnect: TNotifyEvent;

    constructor Create;
    destructor Destroy; override;

    procedure SetAddress(const AddressString: AnsiString);

    procedure Connect;
    procedure Close;

    procedure SendPacket(const Packet: ILineage2CustomClientPacket);
  end;

implementation

function IsSocketError(ResultCode: Integer): Boolean; forward;
function IsSocketNoError(ResultCode: Integer): Boolean; forward;
function IsAsyncError(ResultCode: Integer): Boolean; forward;
procedure CheckSocketResult(ResultCode: Integer; const Op: String); forward;

function LockSharedSocketBuffer : PLineage2Buffer; forward;
procedure UnlockSharedSocketBuffer(Buffer: PLineage2Buffer); forward;

const
  NULL_WINDOW = 0;

{ TLineage2CustomClient }

constructor TLineage2CustomClient.Create;
begin
  inherited;

  SocketHandle:= INVALID_SOCKET;

  WindowHandle:= AllocateHWnd(WndProc);
  if WindowHandle = NULL_WINDOW then
    RaiseLastOSError;

  PacketQueue:= TList<ILineage2CustomClientPacket>.Create;
end;

destructor TLineage2CustomClient.Destroy;
begin
  CloseSocketHandle(False);
  CloseWindowHandle;
  FreeAndNil(PacketQueue);
  inherited;
end;

procedure TLineage2CustomClient.CloseSocketHandle(SetupAsync: Boolean);
begin
  if SocketHandle <> INVALID_SOCKET then
  begin
    CheckSocketResult(closesocket(SocketHandle), 'closesocket');
    SocketHandle:= INVALID_SOCKET;

    if SetupAsync then
      SetupDelayedDisconnectEvent
    else DoNotify(OnDisconnect);
  end;
end;

procedure TLineage2CustomClient.CloseWindowHandle;
begin
  if WindowHandle <> NULL_WINDOW then
  begin
    DeallocateHWnd(WindowHandle);
    WindowHandle:= NULL_WINDOW;
  end;
end;

procedure TLineage2CustomClient.CMSocketMessage(var Message: TCMSocketMessage);

  function CheckError: Boolean;
  var
    ErrorCode: Integer;
  begin
    ErrorCode:= Message.SelectError;
    if ErrorCode <> 0 then
    begin
      CloseSocketHandle;
      Result := False;
    end
    else
      Result := True;
  end;

  function EventToString(Event: Integer) : String;
  begin
    case Message.SelectEvent of
      FD_CONNECT: Result:= 'FD_CONNECT';
      FD_CLOSE: Result:= 'FD_CLOSE';
      FD_READ: Result:= 'FD_READ';
      FD_WRITE: Result:= 'FD_WRITE';
    else Result:= 'FD_UNKNOWN';
    end;
  end;

begin
  if SocketHandle = INVALID_SOCKET then Exit;
  Assert(Message.Socket = SocketHandle);

  if CheckError then
  begin
    case Message.SelectEvent of
      FD_CONNECT: EventConnect;
      FD_CLOSE: EventDisconnect;
      FD_READ:
      begin
        NeedRead:= True;
        EventRead;
      end;
      FD_WRITE:
      begin
        WriteBlocked:= False;
        EventWrite;
      end;
    end;
  end;
end;

procedure TLineage2CustomClient.WMAsyncDisconnect(var Message: TMessage);
begin
  Connecting:= False;
  DoNotify(OnDisconnect);
end;

procedure TLineage2CustomClient.WMPushWrite(var Message: TMessage);
begin
  IsWritePushed:= False;
  EventWrite;
end;

procedure TLineage2CustomClient.EventConnect;
begin
  Connecting:= False;
  Connected;
  DoNotify(OnConnect);
end;

procedure TLineage2CustomClient.EventDisconnect;
begin
  CloseSocketHandle;
end;

procedure TLineage2CustomClient.EventRead;

  // split every packet into random number of partitions (for debug)
  procedure ProcessReadDebug(Buffer: PLineage2Buffer);  
  var
    VirtualPartition: TLineage2Buffer;
  begin    
    while not Buffer.IsEmpty do
    begin
      VirtualPartition.Reset;
      VirtualPartition.ReadFrom(Buffer, 1 + Random(Buffer.ReadRemaining));

      ProcessRead(@VirtualPartition);
    end;
  end;

var
  Buffer: PLineage2Buffer;
  DataLen: Integer;
begin
  if Reading then Exit;
  Buffer:= LockSharedSocketBuffer;
  Reading:= True;
  try
    while NeedRead do
    begin
      NeedRead:= False;

      DataLen:= recv(SocketHandle, Buffer.GetWritePtr^, Buffer.WriteRemaining, 0);

      if IsSocketNoError(DataLen) then
      begin
        Buffer.SeekWritePosition(DataLen);
        {$IFDEF DEBUG}ProcessReadDebug{$ELSE}ProcessRead{$ENDIF}(Buffer);
        Buffer.Reset;
      end
      else         
      begin
        // should be no async error for read operation 
        Assert(not IsAsyncError(DataLen));
        
        CloseSocketHandle;
        Break;
      end;
    end;
  finally
    Reading:= False;
    UnlockSharedSocketBuffer(Buffer);
  end;
end;

function TLineage2CustomClient.ProcessWritePacket(Data: PLineage2Buffer; const Packet: ILineage2CustomClientPacket): Boolean;
var
  PacketStartPosition, PacketSize, PacketEndPosition: Integer;
begin
  PacketStartPosition:= Data.WritePosition;

  // if we can allocate length for packet length and we successfully writed a packet
  if Data.TrySeekWritePosition(PacketLenSize) and Packet.WriteTo(Data) then
  begin
    // read position should be at the start of a buffer
    // because he doesn't meant to be read while he is in a writing process
    Assert(Data.ReadPosition = 0);


    PacketEndPosition:= Data.WritePosition;
    // set read position at the start of a packet actual data
    Data.SetReadPosition(PacketStartPosition + PacketLenSize);
    // set write position at the end of a packet actual data
    Data.SetWritePosition(PacketEndPosition);

    // now we have a packet actual data representation in Data
    Packet.DoPostProc(Data);


    // update end position after post processing
    PacketEndPosition:= Data.WritePosition;

    PacketSize:= PacketEndPosition - PacketStartPosition;

    // write implementation should write some data (zero-size packets are not allowed to send)
    // and it should not exceed maximum packet size
    Assert((PacketSize > PacketLenSize) and (PacketSize <= MaxFullPacketLen));

    // rollback buffer
    Data.SetWritePosition(PacketStartPosition);

    // overwrite packet length
    Data.WriteW(PacketSize);

    // set read position at the start of packet actual data
    Data.SetReadPosition(PacketStartPosition + PacketLenSize);

    // seek to packet end
    Data.SetWritePosition(PacketEndPosition);

    // debug
    {
    WriteLn('send packet: ');
    Data.PrintDump;
    }

    // perform a post-build action, which is basically an encryption
    EncryptPacket(Data);

    Assert(Data.WritePosition = PacketEndPosition, 'Encrypt is not allowed to change the buffer');

    // rollback read position
    Data.SetReadPosition(0);

    // and we're done
    Result:= True;
  end
  else
  // if an overflow occurred this mean that packet could not be written in buffer completely
  begin
    // if overflow has occurred, then buffer must have been contain some data,
    // which was the cause of overflow or else a write implementation of current packet
    // is writing a packet that is larger than maximum size of buffer (which is a maximum possible size of packet)
    Assert(PacketStartPosition > 0, 'Packet buffer overflow');

    // rollback the buffer
    Data.SetWritePosition(PacketStartPosition);

    Result:= False;
  end;
end;

procedure TLineage2CustomClient.ProcessWrite(Data: PLineage2Buffer);
var
  Packet: ILineage2CustomClientPacket;
begin
  WritedPacketsCount:= 0;
  for Packet in Self.PacketQueue do
    if ProcessWritePacket(Data, Packet) then
      Inc(WritedPacketsCount)
    else
      Break;
end;

procedure TLineage2CustomClient.ProcessWriteDoCleanup;
begin
  if WritedPacketsCount = 0 then Exit;
  PacketQueue.DeleteRange(0, WritedPacketsCount);
  WritedPacketsCount:= 0;
end;

procedure TLineage2CustomClient.EventWrite;
var
  Buffer: PLineage2Buffer;
  Ret: Integer;

  DataPtr: PByte;
  DataSize: Integer;
begin
  if Writing then Exit;
  Buffer:= LockSharedSocketBuffer;
  Writing:= True;
  try
    while not WriteBlocked do
    begin
      // test to determine successfulness of next send call to prevent wasteful buffer filling
      Ret:= send(SocketHandle, nil^, 0, 0);
      
      // if no error occurred then we can fill shared buffer to call send with an actual data
      if IsSocketNoError(Ret) then
      begin
        // filling buffer with packet data
        ProcessWrite(Buffer);
      
        // get pointer to data and data size 
        DataPtr:= Buffer.GetReadPtr;
        DataSize:= Buffer.ReadRemaining;
      
        // we not dealing with socket api without serious matter
        if DataSize > 0 then
        begin
          // if we have filled data - sending it
          Ret:= send(SocketHandle, DataPtr^, DataSize, 0);
          
          // if no error occurred - check that everything actually went well and prepare to next send operation
          if IsSocketNoError(Ret) then
          begin
            // checking non-blocking send behavior, send should work as it described on MSDN
            Assert(Ret = DataSize);
            
            // prepare buffer to next send operation
            Buffer.Reset;

            // cleanup packet queue, this will delete writed packets from packet queue
            ProcessWriteDoCleanup;
          end
          else
          if IsAsyncError(Ret) then
          begin
            raise Exception.Create('write rollback not done');
          end;
        end
        else
          // if we have nothing to send - we should no longer be in here
          Break;
      end;

      if IsAsyncError(Ret) then
      begin
        WriteBlocked:= True;
        Break;
      end
      else
      if IsSocketError(Ret) then 
      begin     
        CloseSocketHandle;
        Break;
      end;
    end;
  finally
    Writing:= False;
    UnlockSharedSocketBuffer(Buffer);
  end;
end;

procedure TLineage2CustomClient.ProcessRead(Data: PLineage2Buffer);

  function TryReadFullPacket(Data: PLineage2Buffer) : Boolean;
  var
    PacketLength: Word;
    PacketStartPosition, PacketEndPosition, SavedWritePosition: Integer;
    PacketID: Byte;
    PacketClass: TLineage2ServerPacketClass;
    PacketInstance: TLineage2CustomServerPacket;
  begin
    if Data.TryPeekPacketLength(PacketLength) and (Data.ReadRemaining >= PacketLength) then
    begin
      // current read position is now at the start of a packet
      PacketStartPosition:= Data.ReadPosition;
      PacketEndPosition:= PacketStartPosition + PacketLength;

      // not calling process function for zero-size packets
      if PacketLength > PacketLenSize then
      begin
        SavedWritePosition:= Data.WritePosition;

        // set a read restriction to the end of a packet
        Data.SetWritePosition(PacketEndPosition);


        Data.SetReadPosition(PacketStartPosition + PacketLenSize);
        // decryption
        DecryptPacket(Data);
        // just checking
        // todo FIXME add this behavior to the buffer internal functions, perhaps create a read-only flag
        Assert(Data.WritePosition = PacketEndPosition, 'Decrypt is not allowed to change the buffer');


        // restore packet read position after decryption (sets it at the start of a packet data)
        Data.SetReadPosition(PacketStartPosition + PacketLenSize);

        PacketID:= Data.ReadB;
        PacketClass:= GetPacketClass(PacketID);

        if Assigned(PacketClass) then
        begin
          PacketInstance:= PacketClass.Create(Self);

          PacketInstance.ReadFrom(Data);

          // now process the packet (logic stuff supposed to be doing here)
          PacketInstance.RunPacket;

          Assert(Data.WritePosition = PacketEndPosition, 'ProcessPacket is not allowed to change the buffer');
        end;


        // restore buffer position
        Data.SetWritePosition(SavedWritePosition);
      end;
      
      Data.SetReadPosition(PacketEndPosition);

      Result:= True;
    end  
    else
      Result:= False;
  end;

  function TryCompleteLength(Data: PLineage2Buffer; out PacketLength: Word) : Boolean;
  var
    PacketLengthPendingSize: Integer;    
  begin
    if not PacketReassembleBuffer.TryPeekPacketLength(PacketLength) then
    begin
      // calculate how many bytes left to complete packet length
      PacketLengthPendingSize:= PacketLenSize - PacketReassembleBuffer.ReadRemaining;
      
      // should be at least one
      Assert(PacketLengthPendingSize >= 1);
      
      // if provided data does not have a required amount of bytes then we cannot complete the length right now
      if not PacketReassembleBuffer.TryReadFrom(Data, PacketLengthPendingSize) then
        Exit(False);
      
      // or else we read a required bytes and now we should be able to get packet length
      Assert(PacketReassembleBuffer.TryPeekPacketLength(PacketLength));
    end;
    Result:= True;
  end;

  function TryCompletePacket(Data: PLineage2Buffer) : Boolean;
  var
    PacketLength: Word;
    PacketDataPendingSize: Integer;    
  begin
    // try to complete length first
    if TryCompleteLength(Data, PacketLength) then
    begin
      PacketDataPendingSize:= PacketLength - PacketReassembleBuffer.ReadRemaining;
      
      // can be zero for zero-size packets
      Assert(PacketDataPendingSize >= 0);

      if PacketReassembleBuffer.TryReadFrom(Data, PacketDataPendingSize) then
      begin
        // now we should be able to read a reassembled full packet and reassembly buffer should contain no data
        Assert(TryReadFullPacket(@PacketReassembleBuffer) and PacketReassembleBuffer.IsEmpty);
        
        // reset the buffer for partition of next packet
        PacketReassembleBuffer.Reset;
        
        Exit(True);
      end;
    end;

    // or else just push data into reassembler buffer, because this data are insufficient to complete a packet
    PacketReassembleBuffer.ReadFrom(Data);
    Result:= False;
  end;

begin
  if PacketReassembleBuffer.IsEmpty or TryCompletePacket(Data) then
  begin      
    // try to work with shared buffer as if it contain full packet to avoid unnecessary memory move
    while TryReadFullPacket(Data) do ;
    // whatever remain - goes to packet reassembler
    PacketReassembleBuffer.ReadFrom(Data);
  end;
  
  // data should be processed completely
  Assert(Data.IsEmpty);
end;

procedure TLineage2CustomClient.WndProc(var Message: TMessage);
begin
  try
    Dispatch(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

procedure TLineage2CustomClient.SetAddress(const AddressString: AnsiString);
begin
  Address:= inet_addr(PAnsiChar(AddressString));
end;

procedure TLineage2CustomClient.SetupDelayedDisconnectEvent;
begin
  Assert(WindowHandle <> NULL_WINDOW);
  Assert(PostMessage(WindowHandle, WM_ASYNC_DISCONNECT, 0, 0));
end;

procedure TLineage2CustomClient.DoNotify(Notify: TNotifyEvent);
begin
  if Assigned(Notify) then
    Notify(Self);
end;

procedure TLineage2CustomClient.Setup;
begin
  // setup flags
  NeedRead:= False;
  WriteBlocked:= True;
  Reading:= False;
  Writing:= False;
  IsWritePushed:= False;

  PacketReassembleBuffer.Reset;

  PacketQueue.Clear;
  WritedPacketsCount:= 0;
end;

procedure TLineage2CustomClient.Connect;
const
  ConnectionAsyncFlags : Integer = FD_CONNECT or FD_READ or FD_WRITE or FD_CLOSE;
var
  sock_addr: TSockAddr;
  Ret: Integer;
begin
  Assert(not Connecting);
  Connecting:= True;

  Setup;

  SocketHandle:= socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if SocketHandle = INVALID_SOCKET then
    CheckSocketResult(-1, 'socket');

  CheckSocketResult(WSAAsyncSelect(SocketHandle, WindowHandle, CM_SOCKETMESSAGE, ConnectionAsyncFlags), 'WSAAsyncSelect');

  sock_addr:= Default(TSockAddrIn);
  sock_addr.sin_family:= AF_INET;
  sock_addr.sin_port:= htons(Port);
  sock_addr.sin_addr.S_addr:= Address;

  Ret:= {$IFNDEF VER210}Winapi.{$ENDIF}Winsock.connect(SocketHandle, sock_addr, SizeOf(sock_addr));
  
  if not IsAsyncError(Ret) then
    if IsSocketNoError(Ret) then  
      EventConnect
    else
      EventDisconnect;
end;

procedure TLineage2CustomClient.Close;
begin
  CloseSocketHandle;
end;

procedure TLineage2CustomClient.SendPacket(const Packet: ILineage2CustomClientPacket);
begin
  PacketQueue.Add(Packet);
  PushWrite;
end;

procedure TLineage2CustomClient.PushWrite;
begin
  if IsWritePushed then Exit;
  Assert(PostMessage(Self.WindowHandle, WM_PUSH_WRITE, 0, 0));
  IsWritePushed:= True;
end;

{ SharedSocketBuffer }

const
  SharedSocketBuffer_LockValue = PLineage2Buffer($DEAD);

threadvar
  SharedSocketBuffer: PLineage2Buffer; // one per thread

function LockSharedSocketBuffer : PLineage2Buffer;
var
  SharedSocketBufferReference:  PLineage2Buffer;
begin
  SharedSocketBufferReference:= SharedSocketBuffer;

  // already locked
  if SharedSocketBufferReference = SharedSocketBuffer_LockValue then
    raise Exception.Create('Shared socket buffer is already locked (possible overlapped buffer usage due window messages processing inside packet read or write routines)')
  else
  // yet not created
  if SharedSocketBufferReference = nil then
    SharedSocketBufferReference:= TLineage2Buffer.Allocate
  else SharedSocketBufferReference.Reset;

  SharedSocketBuffer:= SharedSocketBuffer_LockValue;

  Result:= SharedSocketBufferReference;
end;

procedure UnlockSharedSocketBuffer(Buffer: PLineage2Buffer);
begin
  Assert(SharedSocketBuffer = SharedSocketBuffer_LockValue);
  SharedSocketBuffer:= Buffer;
end;

{ socket utils }

function IsSocketError(ResultCode: Integer): Boolean;
begin
  Result:= ResultCode = SOCKET_ERROR;
end;

function IsSocketNoError(ResultCode: Integer): Boolean;
begin
  Result:= not IsSocketError(ResultCode);
end;

function IsAsyncError(ResultCode: Integer): Boolean;
begin
  Result:= IsSocketError(ResultCode) and (WSAGetLastError = EWOULDBLOCK);
end;
 
procedure CheckSocketResult(ResultCode: Integer; const Op: String);
var
  Error: Integer;
begin
  if IsSocketError(ResultCode) then
  begin
    Error:= WSAGetLastError;
    raise ESocketError.CreateResFmt(@sWindowsSocketError, [SysErrorMessage(Cardinal(Error)), Error, Op]);
  end;
end;

procedure Startup;
var
  WSAData: TWSAData;
begin
  CheckSocketResult(WSAStartup(MAKEWORD(2, 2), WSAData), 'WSAStartup');
end;

procedure Cleanup;
begin
  CheckSocketResult(WSACleanup, 'WSACleanup');
end;

initialization
  Startup;
finalization
  Cleanup;
end.
