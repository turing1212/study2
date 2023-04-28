function [MSEinner, VARinner, turningPoint] = innerCV(X, Y, num_repeats, holdoutRate,max_components)
% [MSEinner, VARinner, turningPoint] = innerCV(X, Y, num_repeats, holdoutRate,max_components)
% inner cross validation using Hold-out
% X, X_train_raw, trainning set of the current fold
% Y, Y_train_raw
% num_repeats, repeating times of inner validation 
% holdoutRate, proportion of data used as validation data
% max_components, the max number of components we want to extract
% MSEinner, MSE of each inner validation the current fold, max_components*num_repeats
% VARinner, variance explained of each inner validation the current fold, (1+size(Y,2))*max_components*num_repeats
% turningPoint, the number of components we extract in each time of inner validation, 1*num_repeats

%% Preallocate space to store results
MSEinner = zeros(max_components,num_repeats);
NumofY = size(Y,2) + 1;
VARinner = cell(NumofY,1);
for i = 1:NumofY
    VARinner{i,1} = zeros(max_components,num_repeats);
end
turningPoint = zeros(1,num_repeats);
%% Inner cross-validation loop
for r = 1:num_repeats
    % Define the inner cross-validation settings
    % rng(?)
    inner_CV = cvpartition(size(X,1),'HoldOut',holdoutRate);
    
    % Split data into training and validation sets for this repeat
    train_idx = training(inner_CV,1);
    val_idx = test(inner_CV,1);
    X_train_inner_raw = X(train_idx,:);
    Y_train_inner_raw = Y(train_idx,:);
    X_val_raw = X(val_idx,:);
    Y_val_raw = Y(val_idx,:);
    
    % standardization
    X_train_inner = zscore(X_train_inner_raw);
    Y_train_inner = zscore(Y_train_inner_raw);
    X_val = (X_val_raw - mean(X_train_inner_raw)) ./ std(X_train_inner_raw);
    Y_val = (Y_val_raw - mean(Y_train_inner_raw)) ./ std(Y_train_inner_raw);
    
    %% Perform partial least squares regression with different numbers of components
    num_components = 1:max_components;
    for c = 1:max_components
        [~,~,~,~,beta,~] = plsregress(X_train_inner,Y_train_inner,num_components(c));
        Y_val_pred = [ones(size(X_val,1),1) X_val]*beta;
        
        % Compute MSE
        MSEinner(c,r) = mean(sum((Y_val-Y_val_pred).^2,2));
        
        % Compute Var explained
        VarExpTotal = sum(var(Y_val_pred)) / sum(var(Y_val));
        VarExpEach = var(Y_val_pred) ./ var(Y_val);
        VARinner{1,1}(c,r) = VarExpTotal;
        for i = 2:NumofY
            VARinner{i,1}(c,r) = VarExpEach(1,i-1);
        end
    end
    
    %% Select the optimal number of components based on validation error, method 3
    slope = (MSEinner(max_components,r) - MSEinner(1,r)) / (max_components-1);
    b = MSEinner(1,r) - slope*1;
    abdistance = abs(slope.*num_components + b - MSEinner(:,r)');
    [~, tp] = max(abdistance);
    turningPoint(1,r) = tp;
end
    
end