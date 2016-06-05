unit Utils.OpenSSL;

interface

uses
  {$IFDEF VER210}
  Windows,
  SysUtils;
  {$ELSE}
  Winapi.Windows,
  System.SysUtils;
  {$ENDIF}

const
  libeay32 = 'libeay32.dll';

// Blowfish

const
  BF_ROUNDS       = 16;
  BF_BLOCK        = 8;

type
  PBF_LONG = ^BF_LONG;
  BF_LONG = Cardinal;

  PBF_KEY = ^BF_KEY;
  BF_KEY = record
    P: array [0..BF_ROUNDS + 2 - 1] of BF_LONG;
    S: array [0..4 * 256 - 1] of BF_LONG;
  end;

procedure BF_set_key(key: PBF_KEY; len: Integer; const data: PByte); cdecl; external libeay32;

procedure BF_encrypt(data: PBF_LONG; const key: PBF_KEY); cdecl; external libeay32;
procedure BF_decrypt(data: PBF_LONG; const key: PBF_KEY); cdecl; external libeay32;

implementation

end.
