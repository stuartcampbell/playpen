function [v,vb,axislabel]=pickvarb(data,x)

% function [v,vb,axislabel]=pickvar(data,x)

if ~exist('data','var')|isempty(data)|~isnumeric(x)|(prod(size(x))~=1),
   disp('Empty data set or non-numeric index x. Command not executed.');
   help pickvarb;
   v=[];
   vb=[];
   axislabel=[];
   return
end

de=data.en(2)-data.en(1);
en=[data.en(1)-de/2 data.en+de/2]; % (1,ne+1)
xx=data.u(x);
switch xx,
   case {1},			% Energy
      v=ones(size(data.det_theta))*data.en;
      vb1=ones(size(data.det_theta))*en; 
      vb2=vb1;
      axislabel=[deblank(data.axis_label(x,:)) ' (meV)'];
   case {2}, 			% mod Q 
      v=spe2modQ(data);
      data.en=en;	% (1,ne+1)
      data.det_theta=data.det_theta-data.det_dtheta/2;
      vb1=spe2modQ(data);
      data.det_theta=data.det_theta+data.det_dtheta;
      vb2=spe2modQ(data);
		axislabel=[deblank(data.axis_label(x,:)) ' (Å^{-1})'];
    case {3},			% TwoTheta
      v=(data.det_theta*180/pi)*ones(size(data.en));
      vb1=((data.det_theta-data.det_dtheta/2)*180/pi)*ones(size(en));
      vb2=((data.det_theta+data.det_dtheta/2)*180/pi)*ones(size(en));
      axislabel=[deblank(data.axis_label(x,:)) ' (deg)'];
   case {4},			% Azimuthal angle
     	if isfield(data,'det_psi'),
        	v=(data.det_psi*180/pi)*ones(size(data.en));
         vb1=((data.det_psi-data.det_dpsi/2)*180/pi)*ones(size(en));
         vb2=((data.det_psi+data.det_dpsi/2)*180/pi)*ones(size(en));  
      else
        	v=NaN*ones(size(data.S));
         vb1=NaN*ones(size(data.det_theta,1),size(en,2));
         vb2=vb1;
      end
     	axislabel=[deblank(data.axis_label(x,:)) ' (deg)'];
	case {5},			% Det. Group No.
     	v=(data.det_group)*ones(size(data.en));
      vb1=(data.det_group-0.5)*ones(size(en));
      vb2=(data.det_group+0.5)*ones(size(en));  
      axislabel=[deblank(data.axis_label(x,:))];
   otherwise,
      v=[];
      vb1=[];
      vb2=[];
     	axislabel=[];
     	disp(['Wrong parameter number ' num2str(x)]);
    	return;
end 
if ~isempty(v),    
	vb=cat(3,vb1,vb2);    
end      
		      
      
      
      
      
      
      