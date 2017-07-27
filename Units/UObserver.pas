unit UObserver;

interface

uses System.Classes;

type
  IObserver = interface
    ['{A3208B98-3F48-40C6-9986-43B6CB8F4A7E}']
    procedure Update(Subject: TObject);
  end;

  ISubject = interface
    ['{CA063853-73A8-4AFE-8CAA-50600996ADEB}']
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify;
  end;
//  TSubjectObserver = class;
//
//  TSubject = class (TObject)
//  private
//    FObservers: TList;
//  protected
//    procedure Change;   //Call this method to dispatch change
//  public
//    procedure RegisterObserver(Observer: TSubjectObserver);
//    procedure UnregisterObserver(Observer: TSubjectObserver);
//  end;
//
//  TSubjectObserver = class (TComponent)
//  private
//    FEnabled: Boolean;
//    FOnChange: TNotifyEvent;
//  protected
//    procedure Change;
//  published
//    property Enabled: Boolean read FEnabled write FEnabled;
//    property OnChange: TNotifyEvent read FOnChange write FOnChange;
//  end;
//
//implementation
//
//procedure TSubject.Change;
//var
//  Obs: TSubjectObserver;
//  I: Integer;
//begin
//  for I := 0 to FObservers.Count - 1 do
//  begin
//    Obs := FObservers[I];
//    if Obs.Enabled then Obs.Change;
//  end;
//end;
//
//procedure TSubject.RegisterObserver(Observer: TSubjectObserver);
//begin
//  if FObservers.IndexOf(Observer) = -1 then
//    FObservers.Add(Observer);
//end;
//
//procedure TSubject.UnregisterObserver(Observer: TSubjectObserver);
//begin
//  FObservers.Remove(Observer);
//end;
//
//procedure TSubjectObserver.Change;
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;
implementation

end.
