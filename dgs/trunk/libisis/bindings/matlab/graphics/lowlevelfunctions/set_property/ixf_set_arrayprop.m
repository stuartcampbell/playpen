function st_local_default = ixf_set_arrayprop(st_local_default,totArg,prop,varargin)
%----------------help for GTK ixf_set_arrayprop----------------------------
%
% call syntax: st_local_default = ixf_set_arrayprop (st_local_default,
% totArg, property, value1, value2, ...)
%
% purpose: set defaults for arrays of values
%
% inputs: 
%       st_local_default = the default structure to be changed, 
%       totArg = total arguments, 
%       property = the property to be changed, 
%       values - the sequential values to be set
%
% output: 
%       st_local_default = the changed default strcuture
%
% example: 
%   IXG_ST_DEFAULT = ixf_set_arrayprop(IXG_ST_DEFAULT, 3, 'color', 'blue', 'red');
%
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%parse
    switch(lower(prop))
        %figure
        case 'fcolor'
            col = varargin;
            st_local_default.figure.fcolor = varargin;                    
        case 'funits'
            units = varargin;
            if (~ischar(units))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'units');
            end            
            st_local_default.figure.funits = varargin;                
        case 'fposition'
            position = varargin;
            if (~isnuermic(position))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'position');
            end
            st_local_default.figure.fposition = varargin; 

        case 'resize'
            resize = varargin;        
            if (~ischar(resize))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'resize');
            end       
            st_local_default.figure.resize = varargin;                    
        case 'papersize'
            papersize = varargin;        
            if (~isnumeric(papersize))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'papersize');
            end               
            st_local_default.figure.paper.papersize = varargin;                    
        case 'paperposition'
            paperposition = varargin;        
            if (~isnumeric(paperposition))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'paperposition');
            end   
            
            st_local_default.figure.paper.paperposition = varargin;                    
        case 'paperunits'
            paperunits = varargin;        
            if (~ischar(paperunits))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'paperunits');
            end               
            st_local_default.figure.paper.paperunits = varargin;                    
        case 'paperorientation'
            paperorientation = varargin;        
            if (~ischar(paperorientation))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'paperorientation');
            end               
            st_local_default.figure.paper.paperorientation = varargin;                    
        case 'papertype'
            papertype = varargin;        
            if (~ischar(papertype))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'papertype');                
            end               
            st_local_default.figure.paper.papertype = varargin; 
        case 'paperpositionmode'
            paperpositionmode = varargin;        
            if (~ischar(papertype))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'paperpositionmode');
            end               
            st_local_default.figure.paper.paperpositionmode = paperpositionmode;            
            %plot
        case 'color'
            col = varargin;
            for i=1:length(col)
                flag = ixf_validate_params('c',col{i});
                if (flag == IXG_ST_STDVALUES.false)
                    ixf_display_error(IXG_ST_ERROR.not_valid_color);
                end
            end
            st_local_default.plot.color = varargin;                    
        case 'punits'
            units = varargin;
            if (~ischar(units))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'units');
            end
            st_local_default.plot.punits = varargin;                                        
                                     
        case 'marker'
            marker = varargin;
            for i = 1:length(marker)
                if (~ischar(marker{i}))
                    ixf_display_error(IXG_ST_ERROR.invalid_character,'marker');
                end
                flag = ixf_validate_params('m',marker{i});
                if (flag == IXG_ST_STDVALUES.false)
                    ixf_display_error(IXG_ST_ERROR.not_valid_markerstyle);
                end
            end
            st_local_default.plot.marker = varargin;                                        
        case 'markersize'
            markersize = varargin;
            for i = 1:length(markersize)
                if (~isnumeric(markersize{i}))
                    ixf_display_error(IXG_ST_ERROR.invalid_number,'markersize');
                end
            end
            st_local_default.plot.markersize = varargin;                    
        case 'linewidth'
            linewidth = varargin;        
            for i = 1:length(linewidth)
                if (~isnumeric(linewidth{i}))
                    ixf_display_error(IXG_ST_ERROR.invalid_number,'linewidth');
                end
            end
            st_local_default.plot.linewidth = varargin;                                        
        case 'linestyle'
            linestyle = varargin;        

            for i=1:length(linestyle)
                if (~ischar(linestyle{i}))
                    ixf_display_error(IXG_ST_ERROR.invalid_character,'linestyle');
                end       
                flag = ixf_validate_params('l',linestyle{i});
                if (flag == IXG_ST_STDVALUES.false)
                    ixf_display_error(IXG_ST_ERROR.not_valid_linestyle);
                end
            end
            st_local_default.plot.linestyle = varargin;                                        
            %text
        case 'tcolor'
            st_local_default.text.tcolor = varargin; 
        case 'transparency'
            transparency = varargin;
            for i = 1:length(transparency)
                if (~isnumeric(transparency{i}))
                    ixf_display_error(IXG_ST_ERROR.invalid_number,'transparency');
                end
            end
            st_local_default.plot.edgealpha = transparency;
            st_local_default.plot.facealpha = transparency;
        case 'tunits'
            units = varargin;
            if (~ischar(units))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'units');
            end
            st_local_default.text.tunits = varargin;                     
               
        case 'tposition'
            position = varargin;
            if (~isnuermic(position))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'position');
            end
            st_local_default.text.tposition = varargin; 
                   
        case 'horizontalalignment'
            horizontalalignment  = varargin;
            if (~ischar(horizontalalignment))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'horizontalalignment');
            end
            st_local_default.text.horizontalalignment = varargin;                     
        case 'verticalalignment'
            verticalalignment  = varargin;
            if (~ischar(verticalalignment))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'verticalalignment');
            end
            st_local_default.text.verticalalignment = varargin;                     
        case 'tfontname'
            fontname  = varargin;
            if (~ischar(fontname))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontname');
            end
            st_local_default.text.tfontname = varargin;                     
        case 'tfontsize'
            fontsize  = varargin;
            if (~isnumeric(fontsize))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'fontsize');
            end
            st_local_default.text.tfontsize = varargin;                     
        case 'tfontangle'
            fontangle  = varargin;
            if (~ischar(fontangle))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontangle');
            end
            st_local_default.text.tfontangle = varargin;                     
        case 'tfontweight'
            fontweight  = varargin;
            if (~ischar(fontweight))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontweight');
            end
            st_local_default.text.tfontweight = varargin;                     
        case 'tfontunits'
            fontunits  = varargin;
            if (~ischar(fontunits))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontunits');
            end
            st_local_default.text.tfontunits = varargin;                     
            %for axes
        case 'acolor'
            col = varargin;
            st_local_default.axes.acolor = varargin;
        case 'aunits'
            units = varargin;
            if (~ischar(units))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'units');
            end
            st_local_default.axes.aunits = varargin;
        case 'atag'
            tag = varargin;
            if (~ischar(tag))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'tag');
            end
            st_local_default.axes.atag = varargin;        
        case 'aposition'
            position = varargin;
            if (~isnuermic(position))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'position');
            end
            st_local_default.axes.aposition = varargin;  
        case 'xgrid'
            xgrid = varargin;
            if (~ischar(xgrid))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'xgrid');
            end
            st_local_default.axes.xgrid = varargin; 
        case 'ygrid'
            ygrid = varargin;
            if (~ischar(ygrid))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'ygrid');
            end
            st_local_default.axes.ygrid = varargin;  
        case 'grid'
            grid = varargin;
            if (~ischar(grid))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'grid');
            end
            st_local_default.axes.ygrid = grid;
            st_local_default.axes.xgrid = grid;  
        case 'afontcolor'
            fontcolor = varargin;
            if (~ischar(fontcolor))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontcolor');
            end
            st_local_default.axes.xcolor = fontcolor;
            st_local_default.axes.ycolor = fontcolor;            
        case 'gridlinestyle'
            gridls = varargin;
            if (~ischar(gridls))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'gridls');
            end
            st_local_default.axes.gridlinestyle = gridls;            
  
        case 'afontname'
            fontname  = varargin;
            if (~ischar(fontname))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontname');
            end
            st_local_default.axes.afontname = varargin;         
        case 'afontsize'
            fontsize  = varargin;
            if (~isnumeric(fontsize))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'fontsize');
            end
            st_local_default.axes.afontsize = varargin;          
        case 'afontangle'
            fontangle  = varargin;
            if (~ischar(fontangle))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontangle');
            end
            st_local_default.axes.afontangle = varargin;          
        case 'afontweight'
            fontweight  = varargin;
            if (~ischar(fontweight))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontweight');
            end
            st_local_default.axes.afontweight = varargin;          
        case 'afontunits'
            fontunits  = varargin;
            if (~ischar(fontunits))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'fontunits');
            end
            st_local_default.axes.afontunits = varargin;    
        case 'xlim'
            xlim  = varargin;
            if (~isnumeric(xlim))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'xlim');
            end
            st_local_default.axes.xlim = xlim;
        case 'ylim'
            ylim  = varargin;
            if (~isnumeric(ylim))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'ylim');
            end
            st_local_default.axes.ylim = ylim;
        case 'xscale'
            xscale  = varargin;
            if (~ischar(xscale))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'xscale');
            end
            st_local_default.axes.xscale = xscale;
        case 'yscale'
            yscale  = varargin;
            if (~ischar(yscale))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'yscale');
            end
            st_local_default.axes.yscale = yscale;     
             case 'zlabel'
            zlabel=varargin;
            if (~ischar(zlabel))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'zlabel')
            end
            st_local_default.axes.zlabel=zlabel;
             case 'zgrid'
             zgrid=varargin;
            if (~ischar(zgrid))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'zgrid')
            end
            st_local_default.axes.zgrid=zgrid;
             case 'zscale'
            zscale=varargin;
            if (~ischar(zscale))
                 ixf_display_error(IXG_ST_ERROR.invalid_character,'zscale')
            end
            st_local_default.axes.zscale=zscale;
            case 'zlim'
            zlim=varargin;
            if (~isnumeric(zlim))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'zlim')
            end
            st_local_default.axes.zlim=zlim;
             case 'yaxislocation'            % set orientation of y and x axis
                ylocation=varargin;
                if(~ischar(ylocation))
                    ixf_display_error(IXG_ST_ERROR.invalid_character,'yaxislocation');
                end
                st_local_default.axes.yaxislocation=ylocation;
        case 'xaxislocation'
            xlocation=varargin;
            if(~ischar(xlocation))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'xaxislocation')
            end
            st_local_default.axes.xaxislocation=xlocation;
            
        case 'projection'
            projection=varargin;
            if(~ischar(projection))
                ixf_display_error(IXG_ST_ERROR.invalid_character,'projction')
            end
            st_local_default.axes.projection=projection;            
      case 'drawmode'
            drawmode=varargin;
            if (~ischar(drawmode))
                ixf_display_error(IXT_ST_ERROR.invalid_character,'drawmode')
            end
            st_local_default.axes.drawmode=drawmode;
        case 'colororder'
            colororder=varargin;
            if (~isnumeric(colororder))
                ixf_display_error(IXT_ST_ERROR.invalid_number,'colororder');
            end
            st_local_default.axes.colororder=colororder;
        case 'na'
            % do nothing - but don't display error
        otherwise
            ret = IXG_ST_STDVALUES.false;
            ixf_display_error(IXG_ST_ERROR.wrong_field, varargin{iArgLoop});
    end



