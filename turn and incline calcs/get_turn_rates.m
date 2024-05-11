function turn_rate_reinterp = get_turn_rates(course_data)


%debug
%course_data = this_fit

%Calculate the turning rate of GPS watch data
%Turning rate is in degrees per second and follows right hand rule conventions
% i.e. a negative angle means heading vector is turning to the right!

%course_table
t_posix = course_data.posix_timestamp;
tq = t_posix(1):1:t_posix(end); %just in case we miss a sample
%Hmm what about pauses?

raw_lat = course_data.position_lat/11930465.0;
raw_long = course_data.position_long/11930465.0;
%Garmin stores positions as 32bit signed integers, so divide by (2**32 / 360)

%Strictly it is a touch wasteful to interp if we dont' need to but it's not a big deal
lat_interp = interp1(t_posix, raw_lat, tq, 'linear', 'extrap')'; %Transpose so we can stack cols
long_interp = interp1(t_posix, raw_long, tq, 'linear', 'extrap')';

if length(lat_interp) ~= length(raw_lat)
    warning('At least one sample is missing, using interpolation on lat-long data')
end

p = [long_interp, lat_interp]; 
p_fwd = p(2:end,:) - p(1:end-1,:); 

%Augment w/ first/last points for our cdiff method
p_aug = [p_fwd(1,:); p_fwd; p_fwd(end,:)]; 
p_im1 = p_aug(1:end-1,:); 
p_ip1 = p_aug(2:end,:); 
p_cdiff = (p_ip1 + p_im1)/2; %these are the vectors connecting the points
%i.e. the "heading"

%What we want, though is the signed difference from one to the next
%but done central difference-way so slightly smoother

%Now do central difference on heading
p_cdiff_aug = [p_cdiff(1,:); p_cdiff; p_cdiff(end,:)];
pcd_im1 = p_cdiff_aug(1:end-2,:);  %i+1 vector
pcd_ip1 = p_cdiff_aug(3:end,:);    %i-1 vector
turn_rate = get_directional_angle_vec(pcd_im1, pcd_ip1)/2;
%Divide by two; its the same as averaging the i-1 to i, and the i to i+1 turn rates.

%Now reinterpolate back so it matches course_data size
turn_rate_reinterp = interp1(tq, turn_rate, t_posix, 'linear', 'extrap');

%Wow, it's that easy!?
