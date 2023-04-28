function [cellMSEinner,cellVARinner,turningPoint, ...,
    opti_ncomp, MSEouter, VARouter, Wouter, VIPouter, ...,
    Ypred] = outerCV(X, Y, K, num_repeats, holdoutRate, max_components)
%% Preallocate arrays to store results
cellMSEinner = cell(K,1);
cellVARinner = cell(K,1);
turningPoint = zeros(K,num_repeats);

opti_ncomp = zeros(K,1);
MSEouter = zeros(K,1);
VARouter = zeros(K,size(Y,2)+1);
Wouter = cell(K,1);
VIPouter = cell(K,1);

Ypred = zeros(size(Y,1),size(Y,2));
%% Define the outer cross-validation settings
% rng(?)
outer_CV = cvpartition(size(Y,1),'KFold',K);
%% Outer cross-validation loop
for k = 1:K
    % Split data into training and test sets for this fold
    train_id = training(outer_CV,k);
    test_id = test(outer_CV,k);
    X_train_raw = X(train_id,:);
    Y_train_raw = Y(train_id,:);
    X_test_raw = X(test_id,:);
    Y_test_raw = Y(test_id,:);
    
    % Inner cross-validation loop
    [cellMSEinner{k,1}, cellVARinner{k,1}, turningPoint(k,:)] = innerCV(X_train_raw, Y_train_raw, num_repeats, holdoutRate, max_components);
    
    %% Re-fit model
    % Select the optimal number of components based on mode of turning point, method 3
    opti_ncomp(k) = mode(turningPoint(k,:));
    tabulate(turningPoint(k,:))
    
    % standardize data
    X_train = zscore(X_train_raw);
    Y_train = zscore(Y_train_raw);
    X_test = (X_test_raw -mean(X_train_raw)) ./ std(X_train_raw);
    Y_test = (Y_test_raw -mean(Y_train_raw)) ./ std(Y_train_raw);
    
    % Re-fit the model on the full training set with the optimal number of components
    [XL,YL,XS,~,beta,~,~,stats] = plsregress(X_train,Y_train,opti_ncomp(k));
    
    %% Compute the MSE and Variance explained for this fold
    Y_test_pred = [ones(size(X_test,1),1) X_test]*beta;
    MSEouter(k) = mean(sum((Y_test - Y_test_pred).^2,2));
    VARouter(k,1) = sum(var(Y_test_pred)) / sum(var(Y_test));
    VARouter(k,2:end) = var(Y_test_pred) ./ var(Y_test);
    Wouter{k,1} = stats.W;
    
    % Compute predicted Y for this fold
    Ypred(test_id,:) = Y_test_pred .* std(Y_train_raw) + mean(Y_train_raw);
    
    %% Compute VIP for this fold
    W0 = stats.W ./ sqrt(sum(stats.W.^2,1));
    p = size(XL,1);
    sumSq = sum(XS.^2,1).*sum(YL.^2,1); % VIP for total Y
    VIPscore = sqrt(p* sum(sumSq.*(W0.^2),2) ./ sum(sumSq,2)); 
    sumSqEach = sum(XS.^2,1).* (YL.^2); % VIP for each Y
    m = size(Y_test,2);
    vipScoreEach = zeros(m,p);
    for eachY = 1:m
        sumSqofYi = sumSqEach(eachY,:);
        vipScoreEach(eachY,:) = sqrt(p* sum(sumSqofYi.*(W0.^2),2) ./ sum(sumSqofYi,2));
    end
    VIPouter{k,1} = [VIPscore;vipScoreEach];
end

end
