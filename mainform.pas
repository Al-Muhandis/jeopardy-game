unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, IniPropStorage, DBCtrls,
  ZConnection, ZDataset, RxDBGrid, telegram, tgtypes
  ;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BtnQuestionSend: TButton;
    BtnSendButton: TButton;
    DBNavigator1: TDBNavigator;
    DtSrc: TDataSource;
    EdtAdminChatID: TLabeledEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GrpBxQuestion: TGroupBox;
    EdtTelegramToken: TLabeledEdit;
    IniPrpStrg: TIniPropStorage;
    Label1: TLabel;
    EdtChatID: TLabeledEdit;
    Label2: TLabel;
    MmQuestion: TMemo;
    PgCntrl: TPageControl;
    RxDBGrd: TRxDBGrid;
    Splitter1: TSplitter;
    TbShtGame: TTabSheet;
    TbShtOptions: TTabSheet;
    TglBxReceive: TToggleBox;
    ToolBar1: TToolBar;
    ZCnctn: TZConnection;
    ZQry: TZQuery;
    ZQryID: TAutoIncField;
    ZQryquestion: TStringField;
    ZQryreply: TLargeintField;
    ZQryuser: TStringField;
    procedure BtnSendButtonClick({%H-}Sender: TObject);
    procedure BtnQuestionSendClick({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormDestroy({%H-}Sender: TObject);
    procedure FormShow({%H-}Sender: TObject);
    procedure TglBxReceiveChange(Sender: TObject);
    procedure ZQryreplyGetText(Sender: TField; var aText: string; {%H-}DisplayText: Boolean);
  private
    FCallbackIDCounter: Integer;
    FDoUpdateTelegram: Boolean;
    FTelegramFace: TTelegramFace;
    FTelegramReceiver: TReceiverThread;
    procedure FormAppendMessage(aCallbackQuery: TCallbackQueryObj);
    procedure OpenDB;
  public

  end;

var
  FrmMain: TFrmMain;

implementation

uses
  eventlog, tgutils, StrUtils
  ;

var
  AppDir: String;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
var
  aLogger: TEventLog;
begin
  FTelegramFace:=TTelegramFace.Create;
  FDoUpdateTelegram:=False;
  aLogger:=TEventLog.Create(nil);
  aLogger.LogType:=ltFile;
  aLogger.Active:=True;
  FTelegramFace.Bot.Logger:=aLogger;
  FTelegramFace.Bot.LogDebug:=True;
  FTelegramFace.ReSendInsteadEdit:=False;
  OpenDB;
end;

procedure TFrmMain.BtnQuestionSendClick(Sender: TObject);
var
  aChatID: int64;
begin
  if not FDoUpdateTelegram then
    Exit;
  Cursor:=crHourGlass;
  try
    if EdtTelegramToken.Text<>EmptyStr then
      if TryStrToInt64(Trim(EdtChatID.Text), aChatID) then
      begin
        FTelegramFace.Chat:=aChatID;
        FTelegramFace.Bot.Token:=EdtTelegramToken.Text;
        FTelegramFace.UpdateQuestion(MmQuestion.Text);
      end;
  finally
    Cursor:=crDefault;
  end;
end;

procedure TFrmMain.BtnSendButtonClick(Sender: TObject);
var
  aChatID: int64;
begin
  if not FDoUpdateTelegram then
    Exit;
  Cursor:=crHourGlass;
  try
    if EdtTelegramToken.Text<>EmptyStr then
      if TryStrToInt64(Trim(EdtChatID.Text), aChatID) then
      begin
        FTelegramFace.Chat:=aChatID;
        FTelegramFace.Bot.Token:=EdtTelegramToken.Text;
        FTelegramFace.SendButton(FCallbackIDCounter);
      end;
  finally
    Cursor:=crDefault;
  end;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FTelegramFace.Bot.Logger.Free;
  FTelegramFace.Free;

  if Assigned(FTelegramReceiver) then
  begin
    FTelegramReceiver.Terminate;
    FTelegramReceiver.WaitFor;
  end;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  FDoUpdateTelegram:=True;
end;

procedure TFrmMain.TglBxReceiveChange(Sender: TObject);
begin
  TglBxReceive.Enabled:=False;
  try
    if (Sender as TToggleBox).Checked then
    begin
      FTelegramReceiver:=TReceiverThread.Create(EmptyStr);
      FTelegramReceiver.FreeOnTerminate:=True;
      FTelegramReceiver.OnAppendMessage:=@FormAppendMessage;
      FTelegramReceiver.Bot.Token:=EdtTelegramToken.Text;
      FTelegramReceiver.Start;    
      BtnSendButton.Enabled:=True;
    end
    else begin                     
      BtnSendButton.Enabled:=False;
      FTelegramReceiver.Terminate;
      FTelegramReceiver.WaitFor;
      FTelegramReceiver:=nil;
    end;
  finally
    TglBxReceive.Enabled:=True;
  end;
end;

procedure TFrmMain.ZQryreplyGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  case Sender.AsInteger of
    0: aText:='?';
    1: aText:='Взят';
    2: aText:='Не взят';
  end;
end;

procedure TFrmMain.FormAppendMessage(aCallbackQuery: TCallbackQueryObj);
var
  aID: LongInt;
  aUser: String;
  //aAdminID: int64;
begin
  aID:=StrToIntDef(ExtractDelimited(2, aCallbackQuery.Data, [' ']), 0);
  if aID<>FCallbackIDCounter then
    Exit;
  try
    FTelegramReceiver.Bot.deleteMessage(FTelegramFace.Chat, aCallbackQuery.Message.MessageId);
  except
  end;
  aUser:=CaptionFromUser(aCallbackQuery.From);
  //if TryStrToInt64(EdtAdminChatID.Text, aAdminID) then
    FTelegramFace.Bot.sendMessage(FTelegramFace.Chat, 'Первый: '+aUser);
  Inc(FCallbackIDCounter);
  ZQry.Append;
  ZQryquestion.AsString:=MmQuestion.Text; 
  ZQryuser.AsString:=aUser;
  ZQry.Post;
  ZQry.ApplyUpdates;
end;

procedure TFrmMain.OpenDB;
begin
  ZCnctn.Database:=AppDir+'default.sqlite3';
  ZCnctn.Connected:=True;
  ZQry.Active:=True;
end;

initialization

  AppDir:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));

end.

