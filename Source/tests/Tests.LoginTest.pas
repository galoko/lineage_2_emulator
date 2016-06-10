unit Tests.LoginTest;

interface

uses
  Net.L2LoginClient,
  Utils.Messages;

type
  LoginTest = class sealed
  public
    class procedure DoTest;
  private
    class procedure SocketConnect(Sender: TObject);
    class procedure SocketDisconnect(Sender: TObject);
    class procedure ClientLoginDone(Sender: TObject);
  end;

implementation

{ LoginTest }

class procedure LoginTest.DoTest;
var
  Client: TLineage2LoginClient;
begin
  Client:= TLineage2LoginClient.Create;
  Client.SetAddress('185.71.66.32');
  Client.Port:= TLineage2LoginClient.DEFAULT_LOGIN_PORT;

  Client.Login:= 'testla2tools';
  Client.Password:= 'qwerty';

  Client.OnConnect:= SocketConnect;
  Client.OnDisconnect:= SocketDisconnect;
  Client.OnLoginDone:= ClientLoginDone;
  Client.Connect;

  DispatchMessages;
end;

class procedure LoginTest.SocketConnect(Sender: TObject);
begin
  WriteLn('connected');
end;

class procedure LoginTest.SocketDisconnect(Sender: TObject);
begin
  WriteLn('disconnected');
end;

class procedure LoginTest.ClientLoginDone(Sender: TObject);
begin
  WriteLn('login done, what''s now? nothing else is implemented');
end;

end.
