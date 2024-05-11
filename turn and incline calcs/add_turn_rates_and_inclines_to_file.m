% Demo script that shows how to add turn rate and incline/decline to a pair of .FIT and .GPX files

% Notes:
% - The .FIT and .GPX files can be exported from Garmin Connect or similar training platforms
% - The .FIT file must first be extracted to a raw .csv using the java script in the FIT SDK
% - The raw .csv version of the .FIT file must also be processed using the processing R script
% - Demo data from these previous two steps are not included in this public repo because they contain identifiable data

%Make sure your MATLAB directory is set to /turn an dincline calcs/ before running this script



%Param setup
incline_dist = 25; %meters - how long is our "level" that measures incline? Paper: 25 m 
% (actually doubles to 50m because of cdiff method)
% - you need to use a reasonably large value here because .GPX elevation is truncated to 0.2 m
% See also "coastline paradox"


%Load in bbox regions
bbox_path = 'C:\Users\johnj\Google Drive\IU Grad school\Dissertation\Code\JDX MATLAB\Aim 3 processing\';
bbox_file = 'JDX_RW_flat_hill_turn_bounding_boxes_date_2023_03_16_time_20_46.mat';
bbox_data = load([bbox_path, bbox_file]);
%flat, hill, turn_bboxes

%File paths
data_path = './demo data/';
save_path = './demo data/';


%Find processed file
fit_file = dir([data_path, '*processed.csv']);
assert(all(size(fit_file)==1), 'Cannot find processed FIT file!');
fit_file_name = fit_file.name;
file_path = [data_path, fit_file_name];
course_data = readtable(file_path);

%Get GPX file (a separate export from Garmin Connect)
gpx_dir = dir([data_path, '*.gpx']);
assert(all(size(gpx_dir)==1), 'Cannot find GPX file!');
gpx_file = gpx_dir.name;
gpx_data = gpxread([data_path, gpx_file]);
%Undocumented but you can get elevation easily
%This is survey-based elevation, not from Stryd altimeter


% Get turn rates and raw latlong
turn_rate = get_turn_rates(course_data);
course_data.lat_deg = course_data.position_lat/11930465.0; % (2^32 / 360)
course_data.long_deg = course_data.position_long/11930465.0; % (2^32 / 360)
course_data.turn_rate = turn_rate;



%%  gpx time and elevation to join with course data

%May not need a join, lets make sure its same size though
if length(gpx_data.Time) ~= size(course_data,1)
    warning('FIT and GPX mismatch, interpolating...');
    %If not we have to interpolate - happens if watch was paused
    assert(length(gpx_data.Time) == length(gpx_data.Elevation), 'GPX issue');
    
    %Represent as posixtimes for easy interpolation
    dt = datetime(gpx_data.Time', ...
        'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''', ...
        'TimeZone', 'UTC');
    gpx_posixtimes = posixtime(dt);
    
    %Only need elevation
    elev_interp = interp1(gpx_posixtimes, gpx_data.Elevation', ...
        course_data.posix_timestamp, 'linear', 'extrap');
    
    course_data.elevation_gpx = elev_interp;
else
    course_data.elevation_gpx = gpx_data.Elevation';
end

course_data.incline_pct_grade = get_grade(course_data, incline_dist);


%% Incline plot

% Plot setup
x_limits = [-86.5248, -86.5175];
y_limits = [39.1697, 39.1754];
cmap3 = [[0.0,0.4,0.7];[0.8 0.8 0.8];[1 0 0]];
cmap_interp = flipud(interp1(0:0.5:1, cmap3, linspace(0,1,64)));

figure('units', 'normalized', 'position', [ 0.05 0.05 0.9 0.85]);

%Plot gps track and true path
scatter(course_data.long_deg, course_data.lat_deg, 40, course_data.incline_pct_grade, 'filled');

%incline colorbar
colormap(cmap_interp);
caxis([-15 15])
colorbar();

xlim(x_limits);
ylim(y_limits);
title('Incline/decline');
axis equal;


%% Turn rate plot

figure('units', 'normalized', 'position', [ 0.05 0.05 0.9 0.85]);
scatter(course_data.long_deg, course_data.lat_deg, 40, turn_rate, 'filled');

%Turnrate colorbar
colormap(cmap_interp);
caxis([-20 20])
colorbar();

xlim(x_limits);
ylim(y_limits);
title('Turn rate');
axis equal;

save_name = strrep(fit_file.name, '.csv', '_with_turn_rate_and_incline.csv');

%Save course data
writetable(course_data, [data_path, save_name]);


disp('Data extracted!')
