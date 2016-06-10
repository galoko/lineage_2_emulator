unit Crypt.Login;

interface

uses
  {$IFDEF VER210}
  SysUtils, Math, AnsiStrings,
  {$ELSE}
  System.SysUtils, System.Math, System.AnsiStrings,
  {$ENDIF}
  Utils.L2Buffer, Utils.OpenSSL, Utils.LibGMP;

type
  /// <summary>
  /// Simple wrapper for OpenSSL Blowfish module
  /// </summary>
  TBlowfish = record
  strict private
    Key: BF_KEY;
  public
    /// <summary>
    /// Set blowfish key
    /// </summary>
    procedure SetKey(const Key: array of Byte);
    /// <summary>
    /// Performs data decoding using the Blowfish cbc algorithm without byte-order swapping.
    /// BF_cbc_encrypt performs little-to-big endian swap so it's not working for us
    /// </summary>
    procedure Decode(Data: PLineage2Buffer);
    /// <summary>
    /// Performs data encoding using the Blowfish cbc algorithm without byte-order swapping
    /// BF_cbc_encrypt performs little-to-big endian swap so it's not working for us
    /// </summary>
    procedure Encode(Data: PLineage2Buffer);
  end;

  TLoginCrypt = class
  strict private
    const
      /// <summary>
      /// Key that is used before client received a key from server Init packet
      /// </summary>
	    STATIC_BLOWFISH_KEY : array [0..16 - 1] of Byte = ($6b, $60, $cb, $5b, $82, $ce, $90, $b1, $cc, $2b, $6c, $55, $6c, $6c, $6c, $6c);

      /// <summary>
      /// Size of a checksum block
      /// </summary>
      ChecksumBlockSize = SizeOf(Integer);

    var
      /// <summary>
      /// Blowfish context used to encrypt/decrypt packets
      /// </summary>
      Blowfish: TBlowfish;
      /// <summary>
      /// This flag determine type of key which is in use by now and
      /// it also determines the type of a second decryption pass
      /// </summary>
      IsStatic: Boolean;

      /// <summary>
      /// Public key is used to encrypt login-password pair before it would be sent to server.
      /// The key is received from Init packet
      /// </summary>
      RSAPublicKey: TArray<Byte>;

    function decXORPass(Data: PLineage2Buffer) : Boolean;
    function verifyChecksum(Data: PLineage2Buffer) : Boolean;
  public
    procedure Reset;
    procedure Decrypt(Data: PLineage2Buffer);
    procedure Encrypt(Data: PLineage2Buffer);

    procedure SetBlowfishKey(const Key: TArray<Byte>);
    procedure SetRSAPublicKey(const Key: TArray<Byte>);

    class function CalculateChecksum(Data: PByte; DataLen: Integer) : Integer; static;

    class procedure descrambleModulus(const scrambledMod: TArray<Byte>); static;

    function EncryptAuthData(const Login, Password: AnsiString) : TArray<Byte>;
  end;

implementation

{ TLoginCrypt }

procedure TLoginCrypt.Reset;
begin
  Blowfish.SetKey(TLoginCrypt.STATIC_BLOWFISH_KEY);
  IsStatic:= True;
  RSAPublicKey:= nil;
end;

procedure TLoginCrypt.Decrypt(Data: PLineage2Buffer);
begin
  Blowfish.Decode(Data);

  if IsStatic then
    Assert(decXORPass(Data)) // maybe not worth caring?
  else
    verifyChecksum(Data); // maybe not worth caring?
end;

procedure TLoginCrypt.Encrypt(Data: PLineage2Buffer);
begin
  Blowfish.Encode(Data);
end;

procedure TLoginCrypt.SetBlowfishKey(const Key: TArray<Byte>);
begin
  Blowfish.SetKey(Key);
  IsStatic:= False;
end;

procedure TLoginCrypt.SetRSAPublicKey(const Key: TArray<Byte>);
begin
  Self.RSAPublicKey:= Key;
end;

function TLoginCrypt.EncryptAuthData(const Login, Password: AnsiString): TArray<Byte>;
const
  MAX_LOGIN_LEN = 14;
  MAX_PASSWORD_LEN = 16;
type
  TAuthData = packed record
    DataSize: Byte;
    Unknown: Word;
    Login: array [0..MAX_LOGIN_LEN - 1] of AnsiChar;
    Password: array [0..MAX_PASSWORD_LEN - 1] of AnsiChar;
    Padding: Integer;
  end;

  procedure SetString(var Dst: array of AnsiChar; const Src: AnsiString);
  var
    MoveLen: Integer;
  begin
    MoveLen:= Min(Length(Dst), Length(Src));

    {$IFNDEF VER210}System.AnsiStrings.{$ENDIF}StrMove(PAnsiChar(@Dst[0]), PAnsiChar(Src), MoveLen);
  end;

const
  ResultBlockSize = SizeOf(Integer);

var
  AuthData: TAuthData;

  b: mpz_t;
  e: Cardinal;
  m, r: mpz_t;

  BlockCount: Size_t;

  Ret: Pointer;
begin
  // validate state and input

  Assert(Assigned(Self.RSAPublicKey));

  Assert((Length(Login) <= MAX_LOGIN_LEN) and (Length(Password) <= MAX_PASSWORD_LEN));

  // setup auth data

  AuthData:= Default(TAuthData);
  AuthData.DataSize:= SizeOf(AuthData) - SizeOf(AuthData.DataSize);
  SetString(AuthData.Login, Login);
  SetString(AuthData.Password, Password);

  // init gmp

  mpz_init(b);
  mpz_init(m);
  mpz_init(r);

  try
    // set gmp values

    mpz_import(b, SizeOf(AuthData), 1, 1, 0, 0, AuthData);
    e:= 65537;
    mpz_import(m, Length(Self.RSAPublicKey), 1, 1, 0, 0, Self.RSAPublicKey[0]);
    mpz_powm_ui(r, b, e, m);

    // serialize number into result

    SetLength(Result, Length(Self.RSAPublicKey));

    BlockCount:= Length(Result) div ResultBlockSize;

    Ret:= mpz_export(Result[0], BlockCount, 1, ResultBlockSize, 1, 0, r);
    Assert(Ret = Pointer(Result));

  finally
    // cleanup gmp

    mpz_clear(b);
    mpz_clear(m);
    mpz_clear(r);
  end;
end;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

function TLoginCrypt.decXORPass(Data: PLineage2Buffer) : Boolean;
const
  XorBlockSize = SizeOf(Integer);
  StartPos = XorBlockSize;
  KeyPosFromEnd = XorBlockSize * 2;
var
  DataPtr: PByte;
  DataLen: Integer;

  DataEndPtr: PByte;

  BlockPtr: PInteger;

  Key, BlockCount, Counter: Integer;

  Block: Integer;
begin
  DataPtr:= Data.GetReadPtr;
  DataLen:= Data.ReadRemaining;

  // not padded or less than required to retrieve a key and read something after that
  if (DataLen mod 8 <> 0) or (DataLen <= KeyPosFromEnd) then
    Exit(False);

  // seek to data end
  DataEndPtr:= DataPtr + DataLen;
  // seek back to key
  BlockPtr:= PInteger(DataEndPtr - KeyPosFromEnd);

  // get key
  Key:= BlockPtr^;

  BlockCount:= (DataLen - KeyPosFromEnd - StartPos) div XorBlockSize;
  for Counter := BlockCount downto 1 do
  begin
    Dec(BlockPtr);

    Block:= BlockPtr^;

    Block:= Block xor Key;

    Key:= Key - Block;

    BlockPtr^:= Block;
  end;

  Result:= True;
end;

class function TLoginCrypt.CalculateChecksum(Data: PByte; DataLen: Integer): Integer;
var
  BlockCount, Checksum, Counter: Integer;
  BlockPtr: PInteger;
  Block: Integer;
begin
  BlockCount:= DataLen div ChecksumBlockSize;

  Checksum:= 0;
  BlockPtr:= PInteger(Data);
  for Counter := BlockCount downto 1 do
  begin
    Block:= BlockPtr^;
    Checksum:= Checksum xor Block;
    Inc(BlockPtr);
  end;

  Result:= Checksum;
end;

class procedure TLoginCrypt.descrambleModulus(const scrambledMod: TArray<Byte>);
var
  i: Integer;
  temp: Byte;
begin
  // step 4 : xor last 0x40 bytes with  first 0x40 bytes
  for i:= 0 to $40 - 1 do
    scrambledMod[$40 + i]:= byte(scrambledMod[$40 + i] xor scrambledMod[i]);
  // step 3 : xor bytes 0x0d-0x10 with bytes 0x34-0x38
  for i:= 0 to 4 - 1 do
    scrambledMod[$0d + i]:= byte(scrambledMod[$0d + i] xor scrambledMod[$34 + i]);
  // step 2 : xor first 0x40 bytes with  last 0x40 bytes
  for i:= 0 to $40 - 1 do
    scrambledMod[i]:= byte(scrambledMod[i] xor scrambledMod[$40 + i]);
  // step 1 : 0x4d-0x50 <-> 0x00-0x04
  for i:= 0 to 4 - 1 do
  begin
    temp:= scrambledMod[i];
    scrambledMod[i]:= scrambledMod[$4d + i];
    scrambledMod[$4d + i]:= temp;
  end;
end;

function TLoginCrypt.verifyChecksum(Data: PLineage2Buffer): Boolean;
var
  DataPtr: PByte;
  DataLen: Integer;

  Checksum: Integer;

  LastBlockPtr: PInteger;
  Check: Integer;
begin
  DataPtr:= Data.GetReadPtr;
  DataLen:= Data.ReadRemaining;

  if (DataLen mod ChecksumBlockSize <> 0) or (DataLen <= ChecksumBlockSize) then
    Exit(False);

  Checksum:= CalculateChecksum(DataPtr, DataLen - ChecksumBlockSize);

  LastBlockPtr:= PInteger(DataPtr + DataLen - ChecksumBlockSize);
  Check:= LastBlockPtr^;

  Result:= Check = Checksum;
end;

{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

{ TBlowfish }

procedure TBlowfish.SetKey(const Key: array of Byte);
begin
  BF_set_key(@Self.Key, Length(Key), @Key[0]);
end;

procedure TBlowfish.Decode(Data: PLineage2Buffer);
var
  BlockCount: Integer;
  DataPtr: PByte;
  Counter: Integer;
begin
  DataPtr:= Data.GetReadPtr;
  BlockCount:= Data.ReadRemaining div BF_BLOCK;
  for Counter := BlockCount downto 1 do
  begin
    BF_decrypt(PBF_LONG(DataPtr), @Self.Key);
    Inc(DataPtr, BF_BLOCK);
  end;
end;

procedure TBlowfish.Encode(Data: PLineage2Buffer);
var
  BlockCount: Integer;
  DataPtr: PByte;
  Counter: Integer;
begin
  DataPtr:= Data.GetReadPtr;
  BlockCount:= Data.ReadRemaining div BF_BLOCK;
  for Counter := BlockCount downto 1 do
  begin
    BF_Encrypt(PBF_LONG(DataPtr), @Self.Key);
    Inc(DataPtr, BF_BLOCK);
  end;
end;

end.
