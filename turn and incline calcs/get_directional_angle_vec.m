function angles_deg = get_directional_angle_vec(V1, V2)

%Get directional angle, in degrees, between sets of 2D vectors stored as
% rows in V1 and V2

%Returned angle follows right hand rule sign conventions and represents the angle
%FROM v1 TO v2, for all rows in V1 and V2. 

%Results are normalized to -180 to 180 which DOES matter for turns in certain quadrants

angles_between = atan2(V2(:,2), V2(:,1)) - atan2(V1(:,2), V1(:,1));

%normalized to -180 to 180
if any(angles_between > pi)
    angles_between(angles_between > pi) = angles_between(angles_between > pi) - 2*pi;
end

if any(angles_between <= -pi)
    angles_between(angles_between <= -pi) = angles_between(angles_between <= -pi) + 2*pi;
end

%For debug
% fprintf('Pre-norm | Post-norm\n');
% for a=1:size(angles_between,1)
%     fprintf('%.2f | %.2f\n', ...
%         rad2deg(non_norm_angles(a)), ...
%         rad2deg(angles_between(a)));
% 
% end

angles_deg = rad2deg(angles_between);


