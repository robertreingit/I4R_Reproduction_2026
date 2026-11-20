function get_accuracy_data()
% Quick and dirty function to extract variables.
%
% INPUT:
% OUTPUT:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

    % get cell variables in workspace
    var_lst = evalin('base', "who('ACUR*')");
    var_lst = var_lst(strcmp({var_lst.class}, 'cell'));

    % number of variables
    no_vars = numel(var_lst);
    
    % cm_to_knee, peak_knee, peak_hip_vel, rom_hip, rom_knee
    target_var_names = {...
        %'peak_rel_V_impact_perna', ...
		'V_rel_perna_impact', ...
        'peak_V_ang_perna', ...
        'quad_nd_V_peak', ...
        'ROM hip', ...
        'ROM knee'
        };

    no_dep_vars = numel(target_var_names);

    res_mat = zeros(no_vars, no_dep_vars + 1);

    for i = 1:no_vars
        v_name = var_lst(i).name;
        fprintf('%s\n', v_name);
        cur_var = evalin('base', v_name);
        
        % get struct
        i_det = get_var_position(string(cur_var(1,:)), 'deterministic results');
        stct = cur_var{2, i_det};
        v_names = string(stct(1,:));
        for j = 1:no_dep_vars
            i_pos = get_var_position(v_names, target_var_names{j});
            res_mat(i,j) = stct{2,i_pos};
        end
        
        i_vel = get_var_position(string(cur_var(1,:)), 'Vel. linear 3D');
        stct_vel = cur_var{2, i_vel};
        i_metatarso = get_var_position(string(stct_vel(1,:)), '5 metatarso');
        res_mat(i,6) = stct_vel{2, i_metatarso}(end);

    end

    % Write results to file
    out_var_names = {'cm_to_knee', 'peak_knee', 'peak_hip_vel', ...
        'rom_hip', 'rom_knee', 'metatarsal'};
    fname = 'palucci_data.csv';
    data_tbl = array2table(res_mat, 'VariableNames', out_var_names);
    name_tbl = table(string({var_lst.name})', 'VariableNames',{'var_name'});
    writetable([name_tbl, data_tbl], fname);
    
end

function i_pos = get_var_position(string_vec, name)
% Return the position of name in string_vec
    i_pos = find(string_vec == name);
end
