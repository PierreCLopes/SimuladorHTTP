unit uFormSimuladorHTTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections, System.DateUtils, StrUtils,
  System.JSON, IdHashMessageDigest, Vcl.Menus;

type
  TProduto = record
    Nome: String;
    Valor: Currency;
  end;

  Constante = record
    Const
      URL = 'https://www.api.com/produto';
      Host = 'https://www.api.com';
      URLHttp = 'http://www.api.com/produto';
  end;

  TFormPrincipal = class(TForm)
    Metodo: TComboBox;
    LMetodo: TLabel;
    URL: TEdit;
    LURL: TLabel;
    Executar: TButton;
    Body: TMemo;
    LBody: TLabel;
    Request: TMemo;
    LRequest: TLabel;
    Response: TMemo;
    LResponse: TLabel;
    Key: TEdit;
    LHeader: TLabel;
    LKey: TLabel;
    Value: TEdit;
    LValue: TLabel;
    MainMenu: TMainMenu;
    Instrues1: TMenuItem;
    Protocolo1: TMenuItem;
    Instrues2: TMenuItem;
    Instruessimplificadas1: TMenuItem;
    procedure ExecutarClick(Sender: TObject);
    procedure MetodoChange(Sender: TObject);
    procedure Instrues2Click(Sender: TObject);
    procedure Protocolo1Click(Sender: TObject);
    procedure Instruessimplificadas1Click(Sender: TObject);
  private
    Produto: TDictionary<Integer,TProduto>;

    function GetEndPoint(): String;
    function GetHost(): String;
    function GetUTC(const prDate: TDateTime): String;
    function GetJsonProduto(prID: Integer): String;
    function GetJsonBadRequest(): String;
    function GetJsonMethodNotAllowed(): String;
    function GetJsonNotFound(const prMensagem: String): String;
    function GetJsonUnauthorized(): String;
    function GetHash(): String;

    procedure RequestExecute();
    procedure ResponseExecute();
    procedure CadastrarProduto();
    procedure DeletarProduto(var prResponse: String);
    procedure ResponseBadRequest();
    procedure ResponseNotFound(const prMensagem: String);
    procedure ResponseUnauthorized;
    procedure ResponseError;
    procedure PopUp(prTexto, prCaption: String);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.dfm}

{ TForm1 }

procedure TFormPrincipal.CadastrarProduto;
var
  vJson: TJSONObject;
  vKey: Integer;
  vID: Integer;
  vProduto: TProduto;
begin
  vID := 0;
  try
    vJson := TJSONObject.ParseJSONValue(Body.Text) as TJSONObject;

    for vKey in Produto.Keys do
      if vKey > vID then
        vID := vKey;

    vProduto.Nome := vJson.GetValue('nome').Value;
    vProduto.Valor := StrToCurr(vJson.GetValue('valor').Value);
    Produto.Add(vID + 1, vProduto);
  except
    ResponseBadRequest();
  end;
end;

constructor TFormPrincipal.Create(AOwner: TComponent);
var
  vProduto: TProduto;
begin
  inherited;
  Metodo.AddItem('GET',nil);
  Metodo.AddItem('POST',nil);
  Metodo.AddItem('DELETE',nil);

  Produto := TDictionary<Integer,TProduto>.Create;
  vProduto.Nome := 'Produto';
  vProduto.Valor := 10.50;
  Produto.Add(1, vProduto);

  vProduto.Nome := 'Produto teste';
  vProduto.Valor := 200.35;
  Produto.Add(2, vProduto);

  Key.Text := 'Token';
  Value.Text := '123456789';

  URL.Text := Constante.URL;
end;

procedure TFormPrincipal.DeletarProduto(var prResponse: String);
var
  vJson: TJSONObject;
  vID: Integer;
begin
  vID := 0;
  vJson := nil;
  try
    vJson := TJSONObject.ParseJSONValue(Body.Text) as TJSONObject;
    vID := StrToInt(vJson.GetValue('id').Value);
    prResponse := '{' + sLineBreak +
                  '  "ID": ' + IntToStr(StrToInt(vJson.GetValue('id').Value)) + ',' + sLineBreak +
                  '  "nome": "' + Produto.Items[StrToInt(vJson.GetValue('id').Value)].Nome + '",' + sLineBreak +
                  '  "valor": "' + CurrToStr(Produto.Items[StrToInt(vJson.GetValue('id').Value)].Valor) + '",' + sLineBreak +
                  '}' + sLineBreak;
    Produto.Remove(StrToInt(vJson.GetValue('id').Value));
  except
    if (not Produto.ContainsKey(vID)) and (vJson <> nil) then
      ResponseNotFound('Produto não encontrado.')
    else
      ResponseBadRequest();
  end;
end;

procedure TFormPrincipal.ExecutarClick(Sender: TObject);
begin
  if URL.Text = '' then
  begin
    ShowMessage('URL deve ser informada!');
    URL.SetFocus;
  end
  else
  if Metodo.Text = '' then
  begin
    ShowMessage('Método deve ser informado!');
    Metodo.SetFocus;
  end
  else
  begin
    Response.Clear;
    Request.Clear;

    if (URL.Text = Constante.URL) or (URL.Text = Constante.URLHttp) then
    begin
      RequestExecute;
      if (UpperCase(Key.Text) = 'TOKEN') and (Value.Text = '123456789') then
        ResponseExecute
      else
        ResponseUnauthorized;
    end
    else
    if Pos(Constante.Host,URL.Text) > 0 then
    begin
      RequestExecute;
      ResponseNotFound('Recurso não encontrado.');
    end
    else
    begin
      RequestExecute;
      ResponseError;
    end;
  end;

end;

function TFormPrincipal.GetEndPoint: String;
var
  vResult: String;
begin
  vResult := URL.Text;
  vResult := StringReplace(vResult,'https','',[rfReplaceAll, rfIgnoreCase]);
  vResult := StringReplace(vResult,'http','',[rfReplaceAll, rfIgnoreCase]);
  vResult := StringReplace(vResult,'://','',[rfReplaceAll, rfIgnoreCase]);
  vResult := vResult.Substring(Pos('/',vResult)-1, Length(vResult));

  Result := vResult;
end;

function TFormPrincipal.GetHash: String;
var
  vHash: TIdHashMessageDigest5;
begin
  vHash := TIdHashMessageDigest5.Create;
  Result := vHash.HashStringAsHex('Simulador HTTP');
end;

function TFormPrincipal.GetHost: String;
var
  vResult: String;
begin
  vResult := URL.Text;
  vResult := StringReplace(vResult,'https','',[rfReplaceAll, rfIgnoreCase]);
  vResult := StringReplace(vResult,'http','',[rfReplaceAll, rfIgnoreCase]);
  vResult := StringReplace(vResult,'://','',[rfReplaceAll, rfIgnoreCase]);
  if Pos('/',vResult) > 0 then
    vResult := vResult.Substring(0, Pos('/',vResult)-1);

  Result := vResult;
end;

function TFormPrincipal.GetJsonBadRequest: String;
var
  vResult: String;
begin
  vResult := sLineBreak + '{' + sLineBreak +
                           '  "status": 400,' + sLineBreak +
                           '  "code": "JsonInvalido",' + sLineBreak +
                           '  "mensagem": "Json inválido para o processo desejado."' + sLineBreak +
                           '}';

  Result := vResult;
end;

function TFormPrincipal.GetJsonMethodNotAllowed: String;
var
  vResult: String;
begin
  vResult := sLineBreak + '{' + sLineBreak +
                           '  "status": 405,' + sLineBreak +
                           '  "code": "MetodoNaoSuportado",' + sLineBreak +
                           '  "mensagem": "Método não suportado."' + sLineBreak +
                           '}';

  Result := vResult;
end;

function TFormPrincipal.GetJsonNotFound(const prMensagem: String): String;
var
  vResult: String;
begin
  vResult := sLineBreak + '{' + sLineBreak +
                           '  "status": 404,' + sLineBreak +
                           '  "code": "NaoEncontrado",' + sLineBreak +
                           '  "mensagem": "' + prMensagem + '"' + sLineBreak +
                           '}';

  Result := vResult;
end;

function TFormPrincipal.GetJsonProduto(prID: Integer): String;
var
  vKey: Integer;
  vCount: Integer;
  vResult: String;
begin
  vCount := 0;
  if prID = 0 then
  begin
    vResult := sLineBreak + '[' + sLineBreak;
    for vKey in Produto.Keys do
    begin
      Inc(vCount);
      vResult := vResult + '  {' + sLineBreak +
                           '    "ID": ' + IntTOStr(vKey) + ',' + sLineBreak +
                           '    "nome": "' + Produto.Items[vKey].Nome + '",' + sLineBreak +
                           '    "valor": "' + CurrToStr(Produto.Items[vKey].Valor) + '"' + sLineBreak;
      if Produto.Count = vCount then
        vResult := vResult + '  }' + sLineBreak
      else
        vResult := vResult + '  },' + sLineBreak;
    end;
    vResult := vResult + ']';
  end
  else
  begin
    vResult := sLineBreak + vResult + '{' + sLineBreak +
                                       '  "ID": ' + IntToStr(prID) + ',' + sLineBreak +
                                       '  "nome": "' + Produto.Items[prID].Nome + '",' + sLineBreak +
                                       '  "valor": "' + CurrToStr(Produto.Items[prID].Valor) + '"' + sLineBreak +
                                       '}' + sLineBreak
  end;

  Result := vResult;
end;

function TFormPrincipal.GetJsonUnauthorized: String;
var
  vResult: String;
begin
  vResult := sLineBreak + '{' + sLineBreak +
                           '  "status": 401,' + sLineBreak +
                           '  "code": "NaoAutorizado",' + sLineBreak +
                           '  "mensagem": "Token não encontrado ou inválido."' + sLineBreak +
                           '}';

  Result := vResult;
end;

function TFormPrincipal.GetUTC(const prDate: TDateTime): String;
var
  vResult: String;     
begin
  case DayOfTheWeek(prDate) of
    DayMonday: vResult := 'Mon';
    DayTuesday: vResult := 'Tue';
    DayWednesday: vResult := 'Wed';
    DayThursday: vResult := 'Thu';
    DayFriday: vResult := 'Fri';
    DaySaturday: vResult := 'Sat';
    DaySunday: vResult := 'Sun';
  end; 

  vResult := vResult + ', ' + IntToStr(DayOf(prDate));

  case MonthOf(prDate) of
    MonthJanuary: vResult := vResult + ' Jan';
    MonthFebruary: vResult := vResult + ' Feb';
    MonthMarch: vResult := vResult + ' Mar';
    MonthApril: vResult := vResult + ' Apr';
    MonthMay: vResult := vResult + ' May';
    MonthJune: vResult := vResult + ' Jun';
    MonthJuly: vResult := vResult + ' Jul';
    MonthAugust: vResult := vResult + ' Aug';
    MonthSeptember: vResult := vResult + ' Sep';
    MonthOctober: vResult := vResult + ' Oct';
    MonthNovember: vResult := vResult + ' Nov';
    MonthDecember: vResult := vResult + ' Dec';
  end; 
  
  vResult := vResult + ' ' + IntToStr(YearOf(prDate));
  vResult := vResult + ' ' + IfThen(HourOf(prDate) < 10, '0' + IntToStr(HourOf(prDate)), IntToStr(HourOf(prDate))) + ':' + 
                             IfThen(MinuteOf(prDate) < 10, '0' + IntToStr(MinuteOf(prDate)), IntToStr(MinuteOf(prDate))) + ':' +
                             IfThen(SecondOf(prDate) < 10, '0' + IntToStr(SecondOf(prDate)), IntToStr(SecondOf(prDate))) + ':' + ' GMT';
  
  Result := vResult;   
end;

procedure TFormPrincipal.Instrues2Click(Sender: TObject);
begin
  ShellExecute(Handle,
               'open',
               'https://docs.google.com/document/d/1eHCmKfDalNb6D9JbsngYZvjKHkDOMZzFCOg-JhiaD50/edit?usp=sharing',
               nil,
               nil,
               SW_SHOWMAXIMIZED);
end;

procedure TFormPrincipal.Instruessimplificadas1Click(Sender: TObject);
begin
  PopUp('O simulador possui a estrutura básica para realizar uma ' + sLineBreak +
        'requisição na API, tendo a seleção do Método HTTP (GET, POST, DELETE).' + sLineBreak + sLineBreak +
        'A URL que por padrão será suportado http://www.api.com.br/produto. ' + sLineBreak +
        'Abaixo temos o Header, onde pode ser passado no cabeçalho da mensagem' + sLineBreak +
        'HTTP uma Chave e um Valor dessa chave, por padrão a API irá obrigar um Token,' + sLineBreak +
        'que já é sugerido automaticamente, com a Key “Token” e a Chave “123456789”, ' + sLineBreak +
        'caso não seja informada, será retornado erro de autenticação pelo protocolo HTTP. ' + sLineBreak + sLineBreak +
        'Abaixo temos o Body, onde pode ser informado o corpo da mensagem, ' + sLineBreak +
        'de acordo com o método selecionado, será sugerido um formato de corpo para realizar a requisição. ' + sLineBreak + sLineBreak +
        'Abaixo temos o Request e o Response realizado pelo protocolo HTTP, ' + sLineBreak +
        'onde poderá ser visto todas as informações definidas anteriormente.',
        'Instruções simplificadas');
end;

procedure TFormPrincipal.MetodoChange(Sender: TObject);
var
  vBody: String;
begin
  if Metodo.Text = 'POST' then
  begin
    vBody := '{' + sLineBreak +
             '  "nome": "nome", ' + sLineBreak +
             '  "valor": "0,00" ' + sLineBreak +
             '}';
    Body.Text := vBody;
  end
  else
  if Metodo.Text = 'DELETE' then
  begin
    vBody := '{' + sLineBreak +
             '  "id": 0 ' + sLineBreak +
             '}';
    Body.Text := vBody;
  end;
end;

procedure TFormPrincipal.PopUp(prTexto, prCaption: String);
begin
  with CreateMessageDialog(prTexto, mtInformation, [mbOk]) do
  try
    Caption := prCaption;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormPrincipal.Protocolo1Click(Sender: TObject);
begin
  PopUp('O que é o protocolo HTTP?' + sLineBreak  +
        'HTTP – Protocolo de Transferência de Hipertexto (HyperText Transfer Protocol)' + sLineBreak +
        'Este protocolo funciona através de um modelo computacional conhecido como ' + sLineBreak +
        'cliente-servidor, onde um cliente estabelece a comunicação com um servidor ' + sLineBreak +
        'e ambos passam a trocar informações entre si.' + sLineBreak + sLineBreak +
        'Como funciona o HTTP?' + sLineBreak +
        'Como dito acima, o protocolo utiliza uma estrutura de cliente-servidor, ' + sLineBreak +
        'onde há uma comunicação com mensagens individuais. A mensagem enviada pelo ' + sLineBreak +
        'cliente é chamada de solicitação (request), enquanto a mensagem enviada pelo ' + sLineBreak +
        'servidor é chamada de resposta (response).',

  'Protocolo HTTP');
end;

procedure TFormPrincipal.RequestExecute;
var
  vRequest: String;
begin
  vRequest := Metodo.Text + ' ' + GetEndPoint + ' HTTP/1.1' + sLineBreak +
              'User-Agent: Simulador HTTP 1.0' + sLineBreak +
              'Simulador-Token: ' + GetHash + sLineBreak +
              'Accept-Encoding: gzip, deflate, br' + sLineBreak +
              'Connection: keep-alive' + sLineBreak +
              'Host: ' + GetHost + sLineBreak;
  if (Key.Text <> '') and (Value.Text <> '')then
    vRequest := vRequest + Key.Text + ': ' + Value.Text + sLineBreak;
  if Body.Text <> '' then
  begin
    vRequest := vRequest + 'Content-Type: application/json; charset=utf-8' + sLineBreak +
                           'Content-Length: ' + IntToStr(Length(StringReplace(Body.Text,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                            sLineBreak + Body.Text;
  end;

  Request.Text := vRequest;
end;

procedure TFormPrincipal.ResponseBadRequest;
var
  vResponse: String;
  vJson: String;
begin
  vJson := GetJsonBadRequest();;

  vResponse := vResponse + 'HTTP/1.1 400 Bad Request' + sLineBreak +
                           'Date: ' + GetUTC(Now) + sLineBreak +
                           'Content-Type: application/json; charset=utf-8' + sLineBreak +
                           'Content-Length: ' +IntToStr(Length(StringReplace(vJson,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                           'Connection: keep-alive' + sLineBreak +
                           'Vary: Accept-Encoding' + sLineBreak +
                           'Content-Encoding: br' + sLineBreak +
                           sLineBreak + vJson;

  Response.Text := vResponse;
end;

procedure TFormPrincipal.ResponseError;
begin
  Response.Text := 'Error: getaddrinfo ENOTFOUND ' + GetHost;
end;

procedure TFormPrincipal.ResponseExecute;
var
  vResponse: String;
  vJson: String;
begin
  if Metodo.Text = 'GET' then
  begin
    vJson := GetJsonProduto(0);

    vResponse := vResponse + 'HTTP/1.1 200 OK' + sLineBreak +
                             'Date: ' + GetUTC(Now) + sLineBreak +
                             'Content-Type: application/json; charset=utf-8' + sLineBreak +
                             'Content-Length: ' + IntToStr(Length(StringReplace(vJson,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                             'Connection: keep-alive' + sLineBreak +
                             'Vary: Accept-Encoding' + sLineBreak +
                             'Content-Encoding: br' + sLineBreak +
                             sLineBreak + vJson;
  end
  else
  if Metodo.Text = 'POST' then
  begin
    CadastrarProduto();

    if Response.Text = '' then
    begin
      vJson := GetJsonProduto(Produto.Count);

      vResponse := vResponse + 'HTTP/1.1 200 OK' + sLineBreak +
                               'Date: ' + GetUTC(Now) + sLineBreak +
                               'Content-Type: application/json; charset=utf-8' + sLineBreak +
                               'Content-Length: ' + IntToStr(Length(StringReplace(vJson,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                               'Connection: keep-alive' + sLineBreak +
                               'Vary: Accept-Encoding' + sLineBreak +
                               'Content-Encoding: br' + sLineBreak +
                               sLineBreak + vJson;
    end;
  end
  else
  if Metodo.Text = 'DELETE' then
  begin
    DeletarProduto(vJson);

    if Response.Text = '' then
    begin
      vResponse := vResponse + 'HTTP/1.1 200 OK' + sLineBreak +
                               'Date: ' + GetUTC(Now) + sLineBreak +
                               'Content-Type: application/json; charset=utf-8' + sLineBreak +
                               'Content-Length: ' + IntToStr(Length(StringReplace(vJson,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                               'Connection: keep-alive' + sLineBreak +
                               'Vary: Accept-Encoding' + sLineBreak +
                               'Content-Encoding: br' + sLineBreak +
                               sLineBreak + vJson;
    end;
  end
  else
  begin
    vJson := GetJsonMethodNotAllowed();
    vResponse := vResponse + 'HTTP/1.1 405 Method Not Allowed' + sLineBreak +
                             'Date: ' + GetUTC(Now) + sLineBreak +
                             'Content-Type: application/json; charset=utf-8' + sLineBreak +
                             'Content-Length: ' + IntToStr(Length(StringReplace(vJson,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                             'Connection: keep-alive' + sLineBreak +
                             'Vary: Accept-Encoding' + sLineBreak +
                             'Content-Encoding: br' + sLineBreak +
                             sLineBreak + vJson;
  end;

  if vResponse <> '' then
    Response.Text := vResponse;
end;

procedure TFormPrincipal.ResponseNotFound(const prMensagem: String);
var
  vResponse: String;
  vJson: String;
begin
  vJson := GetJsonNotFound(prMensagem);

  vResponse := vResponse + 'HTTP/1.1 404 Not Found' + sLineBreak +
                           'Date: ' + GetUTC(Now) + sLineBreak +
                           'Content-Type: application/json; charset=utf-8' + sLineBreak +
                           'Content-Length: ' + IntToStr(Length(StringReplace(vJson,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                           'Connection: keep-alive' + sLineBreak +
                           'Vary: Accept-Encoding' + sLineBreak +
                           'Content-Encoding: br' + sLineBreak +
                           sLineBreak + vJson;

  Response.Text := vResponse;
end;

procedure TFormPrincipal.ResponseUnauthorized;
var
  vResponse: String;
  vJson: String;
begin
  vJson := GetJsonUnauthorized();

  vResponse := vResponse + 'HTTP/1.1 401 Unauthorized' + sLineBreak +
                           'Date: ' + GetUTC(Now) + sLineBreak +
                           'Content-Type: application/json; charset=utf-8' + sLineBreak +
                           'Content-Length: ' + IntToStr(Length(StringReplace(vJson,sLineBreak,'',[rfReplaceAll, rfIgnoreCase]))) + sLineBreak +
                           'Connection: keep-alive' + sLineBreak +
                           'Vary: Accept-Encoding' + sLineBreak +
                           'Content-Encoding: br' + sLineBreak +
                           sLineBreak + vJson;

  Response.Text := vResponse;
end;

end.
