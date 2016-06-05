unit Packets.Login;

interface

uses
  {$IFDEF VER210}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  Net.L2Packet, Net.L2LoginClient,
  Crypt.Login,
  Utils.L2Buffer;

type
  TLineage2LoginServerPacket = class(TLineage2CustomServerPacket)
  strict private
    function GetClient : TLineage2LoginClient;
  public
    property Client : TLineage2LoginClient read GetClient;
  end;

  TLineage2LoginClientPacket = class(TLineage2CustomClientPacket)
  protected
    procedure PostProcess(Data: PLineage2Buffer); override;
  end;

  TInit = class(TLineage2LoginServerPacket)
  strict protected
    SessionID, ProtocolRevision: Integer;

    RSAPublicKey, BlowfishKey: TArray<Byte>;

    procedure Read; override;
    procedure Run; override;
  end;

  TAuthGameGuard = class(TLineage2LoginClientPacket)
  strict private
    SessionID: Integer;
  protected
    procedure Write; override;
  public
    constructor Create(Client: TLineage2LoginClient);
  end;

  TGGAuth = class(TLineage2LoginServerPacket)
  strict protected
    GGAuthResponse: Integer;

    procedure Read; override;
    procedure Run; override;
  end;

  TRequestAuthLogin = class(TLineage2LoginClientPacket)
  strict private
    EncryptedAuthData: TArray<Byte>;
    GGAuthResponse: Integer;
  protected
    procedure Write; override;
  public
    constructor Create(Client: TLineage2LoginClient);
  end;

  TLoginFail = class(TLineage2LoginServerPacket)
  strict protected
    ReasonCode: Integer;

    procedure Read; override;
    procedure Run; override;
  end;

  TLoginOk = class(TLineage2LoginServerPacket)
  strict protected
    loginOkID1, loginOkID2: Integer;

    procedure Read; override;
    procedure Run; override;
  end;

  TRequestServerList = class(TLineage2LoginClientPacket)
  strict private
    loginOkID1, loginOkID2: Integer;
  protected
    procedure Write; override;
  public
    constructor Create(Client: TLineage2LoginClient);
  end;

  TServerList = class(TLineage2LoginServerPacket)
  public
    const
      SERVER_FLAG_CHARACTER_CREATION_RESTRICTED = $10;
      SERVER_FLAG_EVENT_SERVER                  = $20;
      SERVER_FLAG_FREE_SERVER                   = $40;

    type
      PServerStruct = ^TServerStruct;

      TServerStruct = packed record
        Id: Byte;
        Ip, Port: Integer;
        AgeLimit: Byte;
        IsPvp: Boolean;
        CurrentOnline, MaxOnline: Word;
        IsDown: Boolean;
        Flags: Integer;
        ShowBrakets: Boolean;
      end;
  strict private
    ServersArray: TArray<TServerStruct>;

    function GetServer(Index: Integer): PServerStruct;
  strict protected
    procedure Read; override;
    procedure Run; override;
  public
    LastServerId: Byte;

    function ServerCount : Integer;
    property Servers[Index: Integer] : PServerStruct read GetServer;

    function FindServerById(ServerId: Byte) : PServerStruct;
  end;

  TRequestServerLogin = class(TLineage2LoginClientPacket)
  strict private
    loginOkID1, loginOkID2: Integer;
    ServerId: Byte;
  protected
    procedure Write; override;
  public
    constructor Create(Client: TLineage2LoginClient);
  end;

  TPlayFail = class(TLineage2LoginServerPacket)
  strict protected
    Reason: Byte;

    procedure Read; override;
    procedure Run; override;
  end;

  TPlayOk = class(TLineage2LoginServerPacket)
  strict protected
    playOkID1, playOkID2: Integer;

    procedure Read; override;
    procedure Run; override;
  end;

implementation

{ TInit }

procedure TInit.Read;
begin
  SessionID:= ReadD;
  ProtocolRevision:= ReadD;
  RSAPublicKey:= ReadB(128);
  Skip('dddd'); // unk GG related?
  BlowfishKey:= ReadB(16);
end;

procedure TInit.Run;
begin
  Assert(ProtocolRevision = $C621); // for now

  TLoginCrypt.descrambleModulus(RSAPublicKey);

  Client.SessionID:= SessionID;
  Client.SetBlowfishKey(BlowfishKey);
  Client.SetRSAPublicKey(RSAPublicKey);
  Client.SendPacket(TAuthGameGuard.Create(Client));
end;

{ TAuthGameGuard }

constructor TAuthGameGuard.Create(Client: TLineage2LoginClient);
begin
  inherited Create;
  Self.SessionID:= Client.SessionID;
end;

procedure TAuthGameGuard.Write;
begin
  SetPacketId($07);
  WriteMask('ddddd', [SessionID, 0, 0, 0, 0]);
end;

{ TGGAuth }

procedure TGGAuth.Read;
begin
  GGAuthResponse:= ReadD;
  Skip('dddd');
end;

procedure TGGAuth.Run;
begin
  Client.GGAuthResponse:= GGAuthResponse;
  Client.SendPacket(TRequestAuthLogin.Create(Client));
end;

{ TRequestAuthLogin }

constructor TRequestAuthLogin.Create(Client: TLineage2LoginClient);
begin
  inherited Create;
  Self.EncryptedAuthData:= Client.GetEncryptedAuthData;
  Self.GGAuthResponse:= Client.GGAuthResponse;
end;

procedure TRequestAuthLogin.Write;
begin
  SetPacketId($00);
  WriteMask('bddddddhc', [EncryptedAuthData, GGAuthResponse, 0, 0, 0, 0, 8, 0, 0]);
end;

{ TLoginFail }

procedure TLoginFail.Read;
begin
  ReasonCode:= ReadD;
end;

procedure TLoginFail.Run;
begin
  raise Exception.Create('not done');
end;

{ TLoginOk }

procedure TLoginOk.Read;
begin
  loginOkID1:= ReadD;
  loginOkID2:= ReadD;
  Skip('dddddd');
  Skip(16);
end;

procedure TLoginOk.Run;
begin
  Client.SetLoginOkID(loginOkID1, loginOkID2);
  Client.SendPacket(TRequestServerList.Create(Client));
end;

{ TRequestServerList }

constructor TRequestServerList.Create(Client: TLineage2LoginClient);
begin
  inherited Create;
  Self.loginOkID1:= Client.loginOkID1;
  Self.loginOkID2:= Client.loginOkID2;
end;

procedure TRequestServerList.Write;
begin
  SetPacketId($05);
  WriteMask('ddc', [loginOkID1, loginOkID2, 4]);
end;

{ TServerList }

procedure TServerList.Read;
var
  ServerCount: Byte;
  Index: Integer;
  Server: PServerStruct;
begin
  ServerCount:= ReadC;
  LastServerId:= ReadC;

  // alloc mem, maybe check? or else High(Byte) * SizeOf(TServerStruct) could be allocated
  SetLength(Self.ServersArray, ServerCount);

  for Index := 0 to Self.ServerCount - 1 do
  begin
    Server:= Self.Servers[Index];

    Server.Id:= ReadC;
    Server.Ip:= ReadD;
    Server.Port:= ReadD;
    Server.AgeLimit:= ReadC;
    Server.IsPvp:= ReadBool;
    Server.CurrentOnline:= ReadH;
    Server.MaxOnline:= ReadH;
    Server.IsDown:= ReadBool;
    Server.Flags:= ReadD;
    Server.ShowBrakets:= ReadBool;
  end;
end;

procedure TServerList.Run;
begin
  Client.ChooseServer(Self);
  Client.SendPacket(TRequestServerLogin.Create(Client));
end;

function TServerList.ServerCount: Integer;
begin
  Result:= Length(Self.ServersArray);
end;

function TServerList.GetServer(Index: Integer): PServerStruct;
begin
  Result:= @Self.ServersArray[Index];
end;

function TServerList.FindServerById(ServerId: Byte): PServerStruct;
var
  Index: Integer;
  Server: PServerStruct;
begin
  for Index := 0 to ServerCount - 1 do
  begin
    Server:= Self.Servers[Index];
    if Server.Id = ServerId then
      Exit(Server);
  end;
  Result:= nil;
end;

{ TRequestServerLogin }

constructor TRequestServerLogin.Create(Client: TLineage2LoginClient);
begin
  inherited Create;
  Self.loginOkID1:= Client.loginOkID1;
  Self.loginOkID2:= Client.loginOkID2;
  Self.ServerId:= Client.ServerId;
end;

procedure TRequestServerLogin.Write;
begin
  SetPacketId($02);
  WriteMask('ddc', [loginOkID1, loginOkID2, ServerId]);
end;

{ TPlayFail }

procedure TPlayFail.Read;
begin
  Reason:= ReadC;
end;

procedure TPlayFail.Run;
begin
  raise Exception.Create('not done');
end;

{ TPlayOk }

procedure TPlayOk.Read;
begin
  playOkID1:= ReadD;
  playOkID2:= ReadD;
end;

procedure TPlayOk.Run;
begin
  Client.SetPlayOkID(playOkID1, playOkID2);
  Client.LoginDone;
end;

// ...

{ TLineage2LoginServerPacket }

function TLineage2LoginServerPacket.GetClient: TLineage2LoginClient;
begin
  Result:= Self.Owner as TLineage2LoginClient;
end;

{ TLineage2LoginClientPacket }

procedure TLineage2LoginClientPacket.PostProcess(Data: PLineage2Buffer);
const
  PacketAlingment = 8;
var
  DataPtr: PByte;
  DataLen, PaddingSize: Integer;

  Checksum: Integer;
begin
  DataPtr:= Data.GetReadPtr;
  DataLen:= Data.ReadRemaining;

  PaddingSize:= PacketAlingment - DataLen mod PacketAlingment;
  if PaddingSize > 0 then
  begin
    WriteZ(PaddingSize);
    // update len
    DataLen:= Data.ReadRemaining;
  end;

  Checksum:= TLoginCrypt.CalculateChecksum(DataPtr, DataLen);

  WriteMask('dddd', [Checksum, 0, 0, 0]); // why zeroes?
end;

end.
