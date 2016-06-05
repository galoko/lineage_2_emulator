unit Net.L2LoginClient;

interface

uses
  {$IFDEF VER210}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  Net.L2CustomClient, Net.L2Packet,
  Crypt.Login,
  Utils.L2Buffer, Utils.OpenSSL;

type
  TLineage2LoginClient = class(TLineage2CustomClient)
  strict private
    LoginCrypt: TLoginCrypt;
  protected
    procedure Connected; override;
    procedure DecryptPacket(Data: PLineage2Buffer); override;
    procedure EncryptPacket(Data: PLineage2Buffer); override;
    function GetPacketClass(PacketID: Byte) : TLineage2ServerPacketClass; override;
  public
    SessionID, GGAuthResponse: Integer;

    /// <summary>
    /// Login to be used in authorization process.
    /// MUST be set by user.
    /// </summary>
    Login: AnsiString;

    /// <summary>
    /// Password to be used in authorization process.
    /// MUST be set by user.
    /// </summary>
    Password: AnsiString;

    /// <summary>
    /// First part of game server authorization key which is received from LoginOk packet
    /// </summary>
    loginOkID1, loginOkID2: Integer;

    ServerId: Byte;

    // auth key second part
    playOkID1, playOkID2: Integer;

    GameServerIp: Integer;
    GameServerPort: Word;

    OnLoginDone: TNotifyEvent;

    constructor Create;
    destructor Destroy; override;

    procedure SetBlowfishKey(const BlowfishKey: TArray<Byte>);
    procedure SetRSAPublicKey(const PublicKey: TArray<Byte>);

    function GetEncryptedAuthData : TArray<Byte>;

    procedure SetLoginOkID(loginOkID1, loginOkID2: Integer);

    procedure ChooseServer(ServerList: TObject);

    procedure SetPlayOkID(playOkID1, playOkID2: Integer);

    procedure LoginDone;
  end;

implementation

uses
  Packets.Login;

{ TLineage2LoginClient }

constructor TLineage2LoginClient.Create;
begin
  inherited;
  LoginCrypt:= TLoginCrypt.Create;
end;

destructor TLineage2LoginClient.Destroy;
begin
  FreeAndNil(LoginCrypt);
  inherited;
end;

procedure TLineage2LoginClient.Connected;
begin
  SessionID:= 0;

  loginOkID1:= 0;
  loginOkID2:= 0;

  ServerId:= 0;

  playOkID1:= 0;
  playOkID2:= 0;

  GameServerIp:= 0;
  GameServerPort:= 0;

  LoginCrypt.Reset;
end;

procedure TLineage2LoginClient.DecryptPacket(Data: PLineage2Buffer);
begin
  LoginCrypt.Decrypt(Data);
end;

procedure TLineage2LoginClient.EncryptPacket(Data: PLineage2Buffer);
begin
  LoginCrypt.Encrypt(Data);
end;

function TLineage2LoginClient.GetPacketClass(PacketID: Byte): TLineage2ServerPacketClass;
begin
  case PacketID of
  $00: Result:= TInit;
  $0B: Result:= TGGAuth;
  $01: Result:= TLoginFail;
  $03: Result:= TLoginOk;
  $04: Result:= TServerList;
  $06: Result:= TPlayFail;
  $07: Result:= TPlayOk;
  else
  begin
    raise ENotSupportedException.Create('Unknown packets are not allowed during development');
    Result:= nil;
  end;
  end;
end;

procedure TLineage2LoginClient.SetBlowfishKey(const BlowfishKey: TArray<Byte>);
begin
  Self.LoginCrypt.SetBlowfishKey(BlowfishKey);
end;

procedure TLineage2LoginClient.SetRSAPublicKey(const PublicKey: TArray<Byte>);
begin
  Self.LoginCrypt.SetRSAPublicKey(PublicKey);
end;

function TLineage2LoginClient.GetEncryptedAuthData: TArray<Byte>;
begin
  Result:= Self.LoginCrypt.EncryptAuthData(Login, Password);
end;

procedure TLineage2LoginClient.SetLoginOkID(loginOkID1, loginOkID2: Integer);
begin
  Self.loginOkID1:= loginOkID1;
  Self.loginOkID2:= loginOkID2;
end;

procedure TLineage2LoginClient.ChooseServer(ServerList: TObject);
var
  List: TServerList;
  Server: TServerList.PServerStruct;
begin
  List:= ServerList as TServerList;

  // TODO more choosy stuff?
  Self.ServerId:= List.LastServerId;

  Server:= List.FindServerById(Self.ServerId);
  Assert(Assigned(Server));

  // save this for further socket adventures
  Self.GameServerIp:= Server.Ip;
  Self.GameServerPort:= Server.Port;
end;

procedure TLineage2LoginClient.SetPlayOkID(playOkID1, playOkID2: Integer);
begin
  Self.playOkID1:= playOkID1;
  Self.playOkID2:= playOkID2;
end;

procedure TLineage2LoginClient.LoginDone;
begin
  DoNotify(OnLoginDone);
end;

end.
