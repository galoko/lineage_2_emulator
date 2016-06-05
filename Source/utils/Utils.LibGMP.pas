unit Utils.LibGMP;

interface

type
  Size_t = NativeUInt;

  mpz_t = record
    /// <summary>
    /// Number of *limbs* allocated and pointed
    /// to by the _mp_d field.
    /// </summary>
    _mp_alloc: Integer;
    /// <summary>
    /// abs(_mp_size) is the number of limbs the
    /// last field points to.  If _mp_size is
    /// negative this is a negative number.
    /// </summary>
    _mp_size: Integer;
    /// <summary>Pointer to the limbs.</summary>
    _mp_d: Pointer;
  end;

const
  libgmp = 'libgmp-10.dll';

procedure mpz_init(var x: mpz_t); cdecl; external libgmp name '__gmpz_init';
procedure mpz_clear(var x: mpz_t); cdecl; external libgmp name '__gmpz_clear';
procedure mpz_powm_ui(var r, b: mpz_t; el: Cardinal; var m: mpz_t); cdecl; external libgmp name '__gmpz_powm_ui';

procedure mpz_import(var z: mpz_t; count: Size_t; order: Integer; size: Size_t; endian: Integer; nail: Size_t; const data); cdecl; external libgmp name '__gmpz_import';
function mpz_export(var data; var countp: Size_t; order: Integer; size: Size_t; endian: Integer; nail: Size_t; const z: mpz_t): Pointer; cdecl; external libgmp name '__gmpz_export';

implementation

end.
