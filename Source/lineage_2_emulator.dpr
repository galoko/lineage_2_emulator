program lineage_2_emulator;

uses
  {$IFDEF VER210}
  Windows,
  {$ELSE}
  Winapi.Windows,
  {$ENDIF}
  Net.L2CustomClient in 'net\Net.L2CustomClient.pas',
  Utils.Messages in 'utils\Utils.Messages.pas',
  Tests.SocketTest in 'tests\Tests.SocketTest.pas',
  Utils in 'utils\Utils.pas',
  Utils.L2Buffer in 'utils\Utils.L2Buffer.pas',
  Net.L2Packet in 'net\Net.L2Packet.pas',
  Tests.LoginTest in 'tests\Tests.LoginTest.pas',
  Net.L2LoginClient in 'net\Net.L2LoginClient.pas',
  Utils.OpenSSL in 'utils\Utils.OpenSSL.pas',
  Packets.Login in 'net\packets\Packets.Login.pas',
  Crypt.Login in 'net\crypt\Crypt.Login.pas',
  Utils.LibGMP in 'utils\Utils.LibGMP.pas';

begin
  {$IFDEF DEBUG}AllocConsole;{$ENDIF}
  // SocketTest.DoTest;
  LoginTest.DoTest;
end.
