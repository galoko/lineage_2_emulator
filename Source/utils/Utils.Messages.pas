unit Utils.Messages;

interface

uses
  {$IFDEF VER210}
  Windows, Messages;
  {$ELSE}
  Winapi.Windows, Winapi.Messages;
  {$ENDIF}

type
  TAsyncCallProc = procedure (Opaque: Pointer);
  TIdleProc = procedure;

procedure DispatchMessages;

implementation

procedure DispatchMessages;
var
  Msg:TMsg;
begin
  repeat
    if PeekMessageW(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      case Msg.Message of
      WM_QUIT: Break;
      else
        TranslateMessage(Msg);
        DispatchMessageW(Msg)
      end;
    end
    else
      WaitMessage;
  until False;
end;

procedure DispatchMessagesWithIdle(const IdleProc: TIdleProc);
var
  Msg: TMsg;
begin
  Assert(Assigned(IdleProc));

  repeat
    if PeekMessageW(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      case Msg.Message of
      WM_QUIT: Break;
      else
        TranslateMessage(Msg);
        DispatchMessageW(Msg)
      end;
    end
    else
      IdleProc();
  until False;
end;

end.
