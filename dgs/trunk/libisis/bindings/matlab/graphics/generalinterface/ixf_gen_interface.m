function [varargout] = ixf_gen_interface(iname,ivalue,fname,fvalue,varargin)
%---------------------------------------------------------------------------
%Function Syntax: ixf_gen_interface(iname,ivalue,fname,fvalue,varargin)
%Input: interface name,value,function name,value,interface
%style,value,varargins
%Output: function returned values 
%Purpose: general interface for all user functions. it is a middle layer
%between user functions and low level functions
%Example:
%ret =
%ixf_gen_interface('iname','show_interface','fname','showrect','istyle',
%IXG_ST_ISTYLE.default);
%[x,y] = ixf_gen_interface('iname','show_interface','fname','show','istyle',
%IXG_ST_ISTYLE.default);
%--------------------------------------------------------------------------

%global structures

[IXG_ST_ERROR, IXG_ST_STDVALUES, IXG_ST_INTERFACEVALIDATION]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_INTERFACEVALIDATION');

%standard arguments
STD_ARG = 4;

%call validate
%if less than call error
%remember nargin does not calculate varargin



%default values
ret = IXG_ST_STDVALUES.true;
iArg = 1;
%total args must be there
permTot = STD_ARG;
%parameters relevant to the function call
totArg = nargin - permTot;
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_gen_interface_nv,nargin,'greater');
%check iname
if (strcmp(iname,'iname'))
    
    switch(ivalue)
        case 'gui_interface'          
            if (strcmp(fname,'fname'))
            %no return values
                switch(fvalue)
                    %parse the function name 
                    case 'menu'
                        %use default
                        ixf_linlog_menu;
                    case 'button'
                        %use default
                        ixf_linlog_button;
                    otherwise
                        ret = IXG_ST_STDVALUES.false;                      
                end
            end
                   
        case 'show_interface'
            if (strcmp(fname,'fname'))
               switch(fvalue)
                    %parse the function name 
                    case 'show'
                        %return two values
                        [varargout{1}, varargout{2}] = ixf_showxy(varargin{:});
                    case 'showrect'
                        %return structure
                        [varargout{1}] = ixf_showrectxy(varargin{:});
                   case 'showtext'
                       [varargout{1}, varargout{2}, varargout{3}] = ixf_showxy(varargin{:});
                   case 'showz'
                       [varargout{1}, varargout{2}, varargout{3}] = ixf_showxyz(varargin{:});
                   case 'showtextz'
                       [varargout{1}, varargout{2}, varargout{3},varargout{4}] = ixf_showxyz(varargin{:});
                   case 'showc'
                       [varargout{1}, varargout{2}, varargout{3}] = ixf_showxyc(varargin{:});
                   case 'showtextc'
                       [varargout{1}, varargout{2}, varargout{3},varargout{4}] = ixf_showxyc(varargin{:});
                       
                    otherwise
                        ret = IXG_ST_STDVALUES.false;                      
                end
            end

            
        case 'getprop_interface'
            if (strcmp(fname,'fname'))
               switch(fvalue)
                    case 'gethandle'
                        %RET = HANDLE
                        [varargout{1}] = ixf_gethandle(varargin{:});
                    case 'getallhandle'
                        %RET = HANDLE
                        [varargout{1}] = ixf_getallhandle(varargin{:});                       
                    case 'checkfigure'
                        %RET = True or false
                        [varargout{1}] = ixf_check_figureapptag(varargin{:});
                   case 'checkforfigure'
                       % return handles of all figures which are not held,
                       % and match the input name and tag or graph type
                       [varargout{1}]=ixf_checkforfigure(varargin{:});
                   case 'getrelatedhandles'
                       [varargout{1}, varargout{2}, varargout{3}, varargout{4}] = ixf_get_related_handles(varargin{:});
                   case 'dataset'
                       varargout{1} = ixf_get_dataset(varargin{:});
                    otherwise
                        ret = IXG_ST_STDVALUES.false;                          
                end
            end
            
            
        case 'setprop_interface'
           %call set property interface
            if (strcmp(fname,'fname'))
               switch(fvalue)
                    %parse the function name 
                    case 'setcurrfig'
                        %RET = HANDLE
                        %check first
                        [varargout{1}] = ixf_setcurrfig(varargin{:});
                    case 'setaxes'
                        %no return                        
                        ixf_setaxes(varargin{:});
                        ixf_redraw_graph(gca);
                   case 'setaspect'
                       ixf_set_aspect(varargin{:});
                   case 'cscale'
                       ixf_set_cscale(varargin{:});
                       
                   case 'color_slider'
                       ixf_add_slider(varargin{:});
                       
                    case 'setappname'
                        %RET = HANDLE
                        [varargout{1}] = ixf_setappname(varargin{:});
                    case 'setdefaultprop'
                        %return is true or false
                        [varargout{1}] = ixf_set_defaultprop(varargin{:});
                    case 'setuserpref'
                        %return is true or false
                        [varargout{1}] = ixf_set_userpref(varargin{:});
                    case 'setlegend'
                        %return true or false
                        [varargout{1}] = ixf_set_legend(varargin{:});
                    case 'resetdefaultprop'
                        %no return
                        [varargout{1}] = ixf_reset_default(varargin{:});
                    case 'setbinning'
                        [varargout{1}] = ixf_set_binning(varargin{:});
                   case 'resetallprop'
                       [varargout{1}] = ixf_reset_all;
                   case 'labelcurrent'
                       % sets the figure label to current (or deletes
                       % label)
                       ixf_setlabel_current(varargin{:});
                   case 'set'
                       ixf_set(varargin{:});
                    otherwise
                        ret = IXG_ST_STDVALUES.false;                          
                end
            end  
            
        case 'plot_interface'
           %call set property interface
           if (strcmp(fname,'fname'))
               
               
               switch(fvalue)
                    %parse the function name 
                    case 'plot'
                        %intialize
                            format = varargin{3};
                            std = 1;
                            if (isempty(format))
                                ixf_display_error(IXG_ST_ERROR.no_format);
                            end
                           %compute according to format
                            [xdata,ydata] = ixf_compute_values(varargin{1}.x, varargin{1}.signal,'',format);
                            [varargout{1},varargout{2},varargout{3}] = ixf_plotfigure(xdata,ydata, varargin{2:end});

                    case 'ploterror'
                        %intialize
                            x = varargin{1}.x;
                            y = varargin{1}.signal;
                            e = varargin{1}.error;
                            format = varargin{3};
                            std = 1;
                            if (isempty(format))
                                ixf_display_error(IXG_ST_ERROR.no_format);
                            end
                            %compute according to format
                            [xdata,ydata] = ixf_compute_values(x,y,e,format);
                            if (~ischar(e))
                                [varargout{1},varargout{2},varargout{3}] = ixf_plotfigure(xdata,ydata, varargin{std+1:end});
                            end
                   case 'ploterrorplus'
                            x = varargin{1}.x;
                            y = varargin{1}.signal;
                            e = varargin{1}.error;
                            format = varargin{3};
                            std = 1;
                            if (isempty(format))
                                ixf_display_error(IXG_ST_ERROR.no_format);
                            end
                            %compute according to format
                            [xdata, ydata] = ixf_compute_values(x,y,e,format);
                            [varargout{1},varargout{2},varargout{3}] = ixf_plotfigure(xdata,ydata, varargin{std+1:end});
                            
                            hold on
                            [xdata,ydata] = ixf_compute_values(x,y,e,'e');
                            if (~ischar(e))
                                [varargout{1},varargout{2},varargout{3}] = ixf_plotfigure(xdata,ydata, varargin{std+1:end}, 'linestyle', '-', 'marker', 'none', 'counter', IXG_ST_STDVALUES.counter_keep);
                            end
                            hold off
                            
                   case 'multiplot'
                        %intialize
                        oneddata = varargin{1};                        
                        [x,y,z] = ixf_multiplot_compute_values(oneddata);
                        [varargout{1},varargout{2},varargout{3}] = ixf_multiplot(x,y,z,varargin{2:end});         
                    
                    %------------------------2d plotting routines
                    
                      case 'area'
                          % varargin{1}=w varargin{2}=format (type)
                          % oid, varargin{3}=format (type)
                          
                          if length(varargin{1}) == 1
                          x=varargin{1}.x;   y=varargin{1}.y;   z=varargin{1}.signal; 
                           [xdata,ydata,zdata]=ixf_compute_values_twod(x,y,z,varargin{2},varargin{3});
                           [varargout{1},varargout{2},varargout{3}]=ixf_multiplot(xdata,ydata,zdata,varargin{2:end});
                          elseif length(varargin{1}) > 1
                                [length_x,length_y] = ixf_get_lengths(varargin{1:end});
                                [xdata,ydata,zdata]=ixf_combine_data(varargin{1},varargin{2},varargin{3},length_x,length_y);
                                [varargout{1},varargout{2},varargout{3}]=ixf_multiplot(xdata,ydata,zdata,varargin{2:end});
                          end
                          
                   case 'patch'
                        if numel(varargin{1}) > 1 
                            [vert, fac, sig] = ixf_combine_data(varargin{1}, varargin{2}, varargin{3}, 1, 1);
                        else
                            vert = varargin{1}.vertices;
                            fac = varargin{1}.faces;
                            sig = varargin{1}.signal;
                        end
                        [varargout{1},varargout{2},varargout{3}]=ixf_patchplot(vert,fac, sig,varargin{2:end});
                                
                   case 'surface'
                            

                                    if length(varargin{1}) == 1
                                        x = varargin{1}.x;  y = varargin{1}.y;  z = varargin{1}.signal;
                                        [x,y,z]=ixf_compute_values_twod(x,y,z,varargin{2},varargin{3});

                                        if size(z,2)==1         % Then only 1 bit of y data, thus only 1 row of data

                                           display('Surface plot of a single row of data is not possible, a line plot has been made instead')
                                           [varargout{1},varargout{2},varargout{3}] = ixf_multiplot(x,y,z,varargin{2:end},'format','twod_line');

                                        else

                                           [varargout{1},varargout{2},varargout{3}]=ixf_multiplot(x,y,z,varargin{2:end});

                                        end    
                                   
                                    elseif length(varargin{1}) > 1                                    
                                
                                        % combine option
                                        [length_x,length_y] = ixf_get_lengths(varargin{1:end});
                                        [xdata,ydata,zdata]=ixf_combine_data(varargin{1},varargin{2},varargin{3},length_x,length_y);
                                        [varargout{1},varargout{2},varargout{3}]=ixf_multiplot(xdata,ydata,zdata,varargin{2:end});
                                
                                     end
                           
               
                   case 'stem'
                       
                           x = varargin{1}.x; y = varargin{1}.y; z = varargin{1}.signal;
                      
                           [x,y,z] = ixf_compute_values_twod(x,y,z,varargin{2},varargin{3});
                           [varargout{1}, varargout{2}, varargout{3}] = ixf_multiplot(x,y,z,varargin{2:end});
                           
                   case 'sliceomatic'
                       
                       x = varargin{1}.x;   y = varargin{1}.y;  z = varargin{1}.z;  signal = varargin{1}.signal;
                       
                       [u1 u2 u3 u4] = ixf_compute_values_threed(x,y,z,signal,varargin{2}, varargin{3});
                       
                       clear x y z signal
                       
                       pack
                       
                       [varargout{1}, varargout{2}, varargout{3}] = ixf_sliceomatic(u1, u2, u3, u4, varargin{2:end});
                                                 
                    otherwise
                        ret = IXG_ST_STDVALUES.false;                          
                    end
            end
        case 'storeinterface'
            
            if (strcmp(fname,'fname'))
                switch(fvalue)
                    case 'keepcmd'
                        %no return 
                        ixf_keepcurr_cmd(varargin{:}, 'keep');
                    case 'releasecmd'
                        ixf_keepcurr_cmd(varargin{:}, 'release');
                    otherwise
                        ret = IXG_ST_STDVALUES.false;                          
                end
            end  
            
            
        case 'undointerface'
            
            if (strcmp(fname,'fname'))
                switch(fvalue)
                    case 'undoplot'
                        %no return 
                        varargout{1} = ixf_undo_plot;
                    otherwise
                        ret = IXG_ST_STDVALUES.false;                          
                end
            end  
            
        otherwise
            %wrong interface passed
            ret = IXG_ST_STDVALUES.false;                         
            ixf_display_error(IXG_ST_ERROR.wrong_interface);        
    end    %end of switch
    %wrong function name passed
    if (ret == IXG_ST_STDVALUES.false)
        ixf_display_error(IXG_ST_ERROR.wrong_function); 
    end    
else
    %wrong arguments in call to gen interface function
    ixf_display_error(IXG_ST_ERROR.wrong_arg);
end % end of main If statement


%----------------------------------------------------------------------
%SubFunctions list
%----------------------------------------------------------------------





