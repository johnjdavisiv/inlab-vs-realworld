function incline_pct_grade = get_grade(course_data, incline_dist)

%Incline dist is "how long is our "level" that we use to measure incline?" 
%Try 25 or 50 meters

%Assumes course_data ALREADY has elevation_gpx field added

incline_pct_grade = nan(size(course_data,1),1);

%Want to do central differencing for elevation gain over distance covered...
for a=1:size(course_data,1)
    if course_data.distance(a) < incline_dist/2
        %First few points we can't use a full window
        start_ix = 1;
    else
        %find the point whose distance covered is closest to current distance - half of incline_dist
        search_dist = course_data.distance(a) - incline_dist/2;
        [~, start_ix] = min(abs(course_data.distance - search_dist));
    end

    if course_data.distance(end) < course_data.distance(a) + incline_dist/2
        end_ix = size(course_data,1);
    else
        %find the point whose distance covered is closest to current distance - half of incline_dist
        search_dist = course_data.distance(a) + incline_dist/2;
        [~, end_ix] = min(abs(course_data.distance - search_dist));
    end

    assert(end_ix > start_ix, 'start-end problem!');
    assert(start_ix ~= end_ix, 'start-end are same!'); %might happen if pause? 

    true_dist_covered = course_data.distance(end_ix) - course_data.distance(start_ix);
    assert(true_dist_covered > 0);
    true_elevation_change = course_data.elevation_gpx(end_ix) - course_data.elevation_gpx(start_ix);

    %Rise over run * 100
    incline_pct_grade(a) = true_elevation_change / true_dist_covered*100;
end
