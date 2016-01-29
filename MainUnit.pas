unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Winapi.ShellAPI, System.IOUtils,
  ExtUnit, Vcl.Imaging.jpeg, Vcl.ExtCtrls;

type
  TFLogonUser = class(TForm)
    EName: TEdit;
    B_Exit: TButton;
    B_OK: TButton;
    ImageFon2: TImage;
    Label1: TLabel;
    procedure B_ExitClick(Sender: TObject);
    procedure ENameChange(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    ClientUserNameEdit:string;
  end;

procedure StartClient;

var
  PathHomeProgramm, PathExchangeFolder, PathSystemRoot, PathUserDesktop:string;
  ClientInfoUserName, ClientInfoPCNames:string;
  TSCA_Client_Version:string;
  CurrentServer:string;
  PresenceFile_RDPFile, PresenceFile_RDPGate, PresenceFile_RemoteAssistant,PresenceFile_RemoteAssistantLink:boolean;
  SetNameClientUser:string;
  FLogonUser: TFLogonUser;

implementation

{$R *.dfm}

procedure TFLogonUser.B_ExitClick(Sender: TObject);
begin
  self.Hide;
  self.Close;
end;

procedure TFLogonUser.B_OKClick(Sender: TObject);
var
  EditTemp:string;
begin
  EditTemp:=Trim(EName.Text);
  if EditTemp <> '' then
  begin
    if Pos('\',EditTemp)>0 then
    begin
      SetNameClientUser:= Copy(EditTemp,Pos('\',EditTemp)+1,Length(EditTemp)-Pos('\',EditTemp));
    end
    else
    begin
      SetNameClientUser:=EditTemp ;
    end;
  end;
  self.Hide;
  self.Close;
end;

procedure TFLogonUser.ENameChange(Sender: TObject);
begin
  if Trim(EName.Text)<>'' then B_OK.Enabled:=true else B_OK.Enabled:=false ;
end;

procedure TFLogonUser.FormShow(Sender: TObject);
begin
  EName.Text:=ClientUserNameEdit;
end;

procedure StartClient;
var
  RDPFileBodyTmp:TStringList;
  RDPTMPFile:string;
  KeyState : TKeyboardState;
begin
  RDPTMPFile:='RDPFileTmp.rdp';
  TSCA_Client_Version:='1.0.1';
  PathHomeProgramm:= Trim(GetEnvironmentVariable('APPDATA')+'\TSCA_Client\') ;
  PathUserDesktop:=Trim(GetEnvironmentVariable('USERPROFILE')+'\Desktop\') ;
  // PathUserDesktop для виндовс XP
  PathSystemRoot:= Trim(GetEnvironmentVariable('SystemRoot')) ;
  PathExchangeFolder:='';
  SetNameClientUser:='';
  ClientInfoUserName:=Trim(GetEnvironmentVariable('USERNAME'));
  ClientInfoPCNames:=Trim(GetEnvironmentVariable('COMPUTERNAME'));
  if TDirectory.Exists(PathHomeProgramm)=False then TDirectory.CreateDirectory(PathHomeProgramm);
  if TDirectory.Exists(PathHomeProgramm+PathExchangeFolder)=False then TDirectory.CreateDirectory(PathHomeProgramm+PathExchangeFolder);
  SetLog('Запуск Client c ПК '+ClientInfoPCNames+' от имени пользователя '+ClientInfoUserName);
  if IniFileRead('ClientVersion') <> TSCA_Client_Version then IniFileWrite('ClientVersion',TSCA_Client_Version) ;
  if IniFileRead('Client Init') = '' then
  begin
    IniFileWrite('Client Init','On') ;
    IniFileWrite('ClientUser','') ;
    IniFileWrite('Cert Install','') ;  //Yes,NotRequired
    IniFileWrite('CertName','') ;
    IniFileWrite('CertSn','') ;
    IniFileWrite('RDPFile','');  //Yes,NotRequired
    IniFileWrite('RDPGate','');
    IniFileWrite('RDPNamePost','');
    IniFileWrite('RDPDomain','');
    IniFileWrite('RDPID','');
    IniFileWrite('RemoteAssistantFile','');  //Yes,NotRequired
    IniFileWrite('RemoteAssistantLink','');
  end;
  if (IniFileRead('RDPFile') <> '') and
     (FileExists(PathHomeProgramm+IniFileRead('RDPFile'))) and
     (IniFileRead('RDPFile') <> 'NotRequired') then PresenceFile_RDPFile:=True else PresenceFile_RDPFile:=false;
  if (IniFileRead('RemoteAssistantFile') <> '') and
     (FileExists(PathHomeProgramm+IniFileRead('RemoteAssistantFile'))) and
     (IniFileRead('RemoteAssistantFile') <> 'NotRequired') then PresenceFile_RemoteAssistant:=True else PresenceFile_RemoteAssistant:=false;
  if (IniFileRead('RemoteAssistantLink') <> '') and
     (FileExists(PathUserDesktop+IniFileRead('RemoteAssistantLink'))) and
     (IniFileRead('RemoteAssistantLink')<> 'NotRequired')  then PresenceFile_RemoteAssistantLink:=True else PresenceFile_RemoteAssistantLink:=false;
  GetKeyboardState(KeyState);
  SetNameClientUser:=IniFileRead('ClientUser');
  if (SetNameClientUser = '') or ((KeyState[VK_SHIFT] And 128) <> 0) then
  begin
    SetLog('Имя пользователя не указано или требует изменения');
    FLogonUser.ClientUserNameEdit:= SetNameClientUser;
    Application.Run;
  end;

  if SetNameClientUser <> '' then
  begin
    SetLog('Имя пользователя: '+SetNameClientUser);
    IniFileWrite('ClientUser',SetNameClientUser);
  end;

  if (IniFileRead('ClientUser') <> '') and (GetParseStringCount(IniFileRead('ServersName')) > 0) then
  begin
    SetLog('Запуск обмена с сервером');
    TSCAServerExchage;
    if FileExists(PathHomeProgramm+RDPTMPFile)=True then
    begin
      DeleteFile(PChar(PathHomeProgramm+RDPTMPFile));
      SetLog('Удален временный файл RDP');
    end;

    if (FileExists(PathSystemRoot+'\system32\mstsc.exe')=True) and (FileExists(PathHomeProgramm+IniFileRead('RDPFile'))=True)  then
    begin
      SetLog('Файл RDP найден');
      RDPFileBodyTmp:=TStringList.Create;
      RDPFileBodyTmp.LoadFromFile(PathHomeProgramm+IniFileRead('RDPFile'));
      if IniFileRead('RDPGate')='No' then
      begin
        RDPFileBodyTmp.Add('full address:s:'+CurrentServer+':'+IniFileRead('RDPNamePost'));
        RDPFileBodyTmp.Add('gatewaycredentialssource:i:4');
        RDPFileBodyTmp.Add('gatewayhostname:s:');
        RDPFileBodyTmp.Add('gatewayusagemethod:i:4');
      end;

      if IniFileRead('RDPGate')='Yes' then
      begin
        RDPFileBodyTmp.Add('full address:s:'+IniFileRead('RDPNamePost'));
        RDPFileBodyTmp.Add('gatewaycredentialssource:i:4');
        RDPFileBodyTmp.Add('gatewayhostname:s:'+CurrentServer);
        RDPFileBodyTmp.Add('gatewayusagemethod:i:2');
      end;
      RDPFileBodyTmp.Add('username:s:'+IniFileRead('RDPDomain')+'\'+IniFileRead('ClientUser'));
      RDPFileBodyTmp.SaveToFile(PathHomeProgramm+RDPTMPFile);
      ShellExecute(0,'open',PChar(PathSystemRoot+'\system32\mstsc.exe'),PChar(PathHomeProgramm+RDPTMPFile),nil,1);
      FreeAndNil(RDPFileBodyTmp);
    end
    else
    begin
      SetLog('Файл RDP НЕ найден '+PathHomeProgramm+IniFileRead('RDPFile'))
    end ;
  end
  else
  begin
    SetLog('Не определено имя пользователя. Завершение работы клиента');
  end;
  SetLog('Остановка клиента');
end;
end.
