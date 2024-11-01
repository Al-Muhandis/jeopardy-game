unit telegram;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgsendertypes, taskworker, tgtypes
  ;

type

  { TTelegramTask }

  TTelegramTask = class(TPersistent)
  private
    FID: Integer;
    FQuestion: String;
    FSendButton: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Question: String read FQuestion write FQuestion;
    property SendButton: Boolean read FSendButton write FSendButton;
    property ID: Integer read FID write FID;
  end;

  TCustomTelegramThread = specialize TgTaskWorkerThread<TTelegramTask>;

  { TTelegramThread }

  TTelegramThread = class(TCustomTelegramThread)
  private
    FBot: TTelegramSender;
    FChat: Int64;  
    FMessageID: Int64;    
    FReSendInsteadEdit: Boolean;
    procedure SendQuestion(const aQuestion: String);
  protected
    procedure ProcessTask(ATask: TTelegramTask); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Chat: Int64 read FChat write FChat;
  end;


  { TTelegramFace }

  TTelegramFace = class
  private
    FThread: TTelegramThread;
    function GetBot: TTelegramSender;
    function GetChat: Int64;
    function GetReSendInsteadEdit: Boolean;
    procedure SetChat(AValue: Int64);
    procedure SetReSendInsteadEdit(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateQuestion(const aQuestion: String);
    procedure SendButton(aButtonID: Integer);
    property Bot: TTelegramSender read GetBot;
    property Chat: Int64 read GetChat write SetChat;
    property ReSendInsteadEdit: Boolean read GetReSendInsteadEdit write SetReSendInsteadEdit;
  end;

  TOnAppendMessage = procedure (aMsg: TCallbackQueryObj) of object;

  { TReceiverThread }

  TReceiverThread=class(TThread)
  private
    FBot: TTelegramSender;
    FOnAppendMessage: TOnAppendMessage;
    FLPTimeout: Integer;
    procedure BotReceiveCallbackQuery({%H-}ASender: TObject; {%H-}ACallback: TCallbackQueryObj);
    procedure BotStartCommandHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure SendMsgToMainThread;
  public
    constructor Create(const AToken: String);
    destructor Destroy; override;
    procedure Execute; override;
    property Bot: TTelegramSender read FBot write FBot;
    property OnAppendMessage: TOnAppendMessage read FOnAppendMessage write FOnAppendMessage;
  end;

implementation

uses
  Graphics, fpjson, eventlog
  ;

{ TReceiverThread }

procedure TReceiverThread.BotReceiveCallbackQuery(ASender: TObject; ACallback: TCallbackQueryObj);
begin
  Synchronize(@SendMsgToMainThread);
end;

procedure TReceiverThread.BotStartCommandHandler(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
begin
  FBot.sendMessage('Это телеграм бот для турнира "Своей игры"');
end;

procedure TReceiverThread.SendMsgToMainThread;
begin
  if Assigned(FOnAppendMessage) then
    FOnAppendMessage(FBot.CurrentUpdate.CallbackQuery);
end;

constructor TReceiverThread.Create(const AToken: String);
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  FBot:=TTelegramSender.Create(AToken);
  FBot.Logger:=TEventLog.Create(nil);
  FBot.Logger.LogType:=ltFile;
  FBot.Logger.FileName:='receiver.log';
  FBot.LogDebug:=True;
  FBot.CommandHandlers['/start']:=@BotStartCommandHandler;
  Fbot.OnReceiveCallbackQuery:=@BotReceiveCallbackQuery;
  FLPTimeout:=6;
end;

destructor TReceiverThread.Destroy;
begin
  FBot.Logger.Free;
  FreeAndNil(FBot);
  inherited Destroy;
end;

procedure TReceiverThread.Execute;
begin
  try
    while not Terminated do
      FBot.getUpdatesEx(0, FLPTimeout);
  except
    on E: Exception do
      FBot.Logger.Error(E.ClassName+': '+ E.Message);
  end;
end;

{ TTelegramTask }


procedure TTelegramTask.Assign(Source: TPersistent);
var
  aSource: TTelegramTask;
begin
  if Source is TTelegramTask then
  begin
    aSource:=TTelegramTask(Source);
    FQuestion:=aSource.Question;
    FSendButton:=aSource.SendButton;
    FID:=aSource.ID;
  end else
    inherited Assign(Source);
end;


{ TTelegramThread }

procedure TTelegramThread.SendQuestion(const aQuestion: String);
begin
  try
    FBot.sendMessage(FChat, aQuestion);
    FMessageID:=(FBot.JSONResponse as TJSONObject).Int64s['message_id'];
  except
    FMessageID:=0;
  end;
end;

procedure TTelegramThread.ProcessTask(ATask: TTelegramTask);
var
  aReplyMarkup: TReplyMarkup;
begin
  aReplyMarkup:=nil;
  try
    try
      if Count>0 then
        Exit;
      if ATask.SendButton then
      begin
        aReplyMarkup:=TReplyMarkup.Create;
        aReplyMarkup.CreateInlineKeyBoard.AddButton('🔴 Я знаю ответ!', 'press '+ATask.ID.ToString);
      end;
      if FMessageID=0 then
        if not ATask.SendButton then
          SendQuestion(ATask.Question)
        else
          FBot.sendMessage(FChat, 'Нажмите кнопку, если знаете ответ как можно быстрее!', pmDefault, False,
            aReplyMarkup)
      else
        if FReSendInsteadEdit then
          if not ATask.SendButton then
          begin
            FBot.deleteMessage(FChat, FMessageID);
            SendQuestion(ATask.Question);
          end
          else
            FBot.editMessageReplyMarkup(FChat, FMessageID, EmptyStr, aReplyMarkup)
        else
          if not ATask.SendButton then
            SendQuestion(ATask.Question)
          else
            FBot.sendMessage(FChat, 'Нажмите кнопку, если знаете ответ как можно быстрее!', pmDefault, False,
              aReplyMarkup);
    finally
      aReplyMarkup.Free;
      ATask.Free;
    end;
  except        { #todo : create thread logger }
    //on E: Exception do Log(etError, E.Classname+': '+E.message);
  end;
end;

constructor TTelegramThread.Create;
begin
  inherited Create;
  FBot:=TTelegramSender.Create(EmptyStr);
  FReSendInsteadEdit:=True;
end;

destructor TTelegramThread.Destroy;
begin
  FreeAndNil(FBot);
  inherited Destroy;
end;

{ TTelegramFace }

function TTelegramFace.GetChat: Int64;
begin
  Result:=FThread.Chat;
end;

function TTelegramFace.GetBot: TTelegramSender;
begin
  Result:=FThread.FBot;
end;

function TTelegramFace.GetReSendInsteadEdit: Boolean;
begin
  Result:=FThread.FReSendInsteadEdit;
end;

procedure TTelegramFace.SetChat(AValue: Int64);
begin
  FThread.Chat:=AValue;
end;

procedure TTelegramFace.SetReSendInsteadEdit(AValue: Boolean);
begin
  FThread.FReSendInsteadEdit:=AValue;
end;

constructor TTelegramFace.Create;
begin
  FThread:=TTelegramThread.Create;
  FThread.FReSendInsteadEdit:=True;
  FThread.Start;
end;

destructor TTelegramFace.Destroy;
begin
  FThread.TerminateWorker;
  FThread.WaitFor;
  FThread.Free;
  inherited Destroy;
end;

procedure TTelegramFace.UpdateQuestion(const aQuestion: String);
var
  aTask: TTelegramTask;
begin
  aTask:=TTelegramTask.Create;
  try
    aTask.Question:=aQuestion;
    aTask.SendButton:=False;
  except
    aTask.Free;
    Exit;
  end;
  FThread.PushTask(aTask);
end;

procedure TTelegramFace.SendButton(aButtonID: Integer);
var
  aTask: TTelegramTask;
begin
  aTask:=TTelegramTask.Create;
  try
    aTask.SendButton:=True;
    aTask.ID:=aButtonID;
  except
    aTask.Free;
    Exit;
  end;
  FThread.PushTask(aTask);
end;

end.

