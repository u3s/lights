-module(robot).

-export([start/1]).

-include_lib("wx/include/wx.hrl").

start(Name) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, Name, [{size, {100, 260}}]),
    Panel = wxPanel:new(Frame),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    VSizer = wxBoxSizer:new(?wxVERTICAL),

    OnPaint = fun(_Evt, _Obj) ->
		      Paint = wxPaintDC:new(Panel),
		      Pen = wxPen:new(),
		      Brush = wxBrush:new(?wxRED),
		      wxPen:setColour(Pen, ?wxBLACK),
		      wxPen:setWidth(Pen, 6),
		      wxDC:setPen(Paint, Pen), 
		      wxDC:setBrush(Paint, Brush),
		      wxDC:drawCircle(Paint, {50,50}, 30),
		      wxBrush:destroy(Brush),
		      Brush1 = wxBrush:new(?wxBLACK),
		      wxDC:setBrush(Paint, Brush1),
		      wxDC:drawCircle(Paint, {50,130}, 30),
		      wxBrush:destroy(Brush1),
		      wxDC:drawCircle(Paint, {50,210}, 30),
		      wxPen:destroy(Pen),
		      wxPaintDC:destroy(Paint)
	      end,

    wxSizer:prependSpacer(Sizer, 10),
    wxSizer:addSpacer(Sizer, 5),
    wxSizer:prependSpacer(VSizer, 10),
    wxSizer:add(VSizer, Sizer),
    wxPanel:setSizer(Panel,VSizer),
    wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
    wxFrame:connect(Panel, close_window),
    wxFrame:center(Frame),
    wxFrame:show(Frame),
    loop(Frame, Panel).

loop(Frame, Panel) ->
    receive
	{R, Y, G} ->
	    OnPaint2 = fun(_Evt, _Obj) ->
			       Paint = wxPaintDC:new(Panel),
			       Pen = wxPen:new(),
			       Brush =
				   case R of
				       on ->
					   wxBrush:new(?wxRED);
				       off ->
					   wxBrush:new(?wxBLACK)
				   end,
			       wxPen:setColour(Pen, ?wxBLACK),
			       wxPen:setWidth(Pen, 6),
			       wxDC:setPen(Paint, Pen), 
			       wxDC:setBrush(Paint, Brush),
			       wxDC:drawCircle(Paint, {50,50}, 30),
			       wxBrush:destroy(Brush),
			       Brush1 =
				   case Y of
				       on ->
					   wxBrush:new({255, 255, 0});
				       off ->
					   wxBrush:new(?wxBLACK)
				   end,	   
			       wxDC:setBrush(Paint, Brush1),
			       wxDC:drawCircle(Paint, {50,130}, 30),
			       wxBrush:destroy(Brush1),
			       Brush2 =
				   case G of
				       on ->
					   wxBrush:new(?wxGREEN);
				       off ->
					   wxBrush:new(?wxBLACK)
				   end,
			       wxDC:setBrush(Paint, Brush2),
			       wxDC:drawCircle(Paint, {50,210}, 30),
			       wxPen:destroy(Pen),
			       wxPaintDC:destroy(Paint)
		       end,
    wxFrame:connect(Panel, paint, [{callback, OnPaint2}]),
    wxFrame:refresh(Frame)
    end,     
    loop(Frame, Panel).   
