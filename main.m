%% Load data
load('meats.mat');
X = meats(:,1:100);
Y = meats(:,101:103);

% CV parameter set
K = 10;
max_components = 20;
num_repeats = 50;
holdoutRate = 0.3;

% test inner
[MSEinner, VARinner, turningPoint] = innerCV(X, Y, num_repeats, holdoutRate,max_components);
[MSEinner, PCTVAR, VARinner, turningPoint] = innerCVtest(X, Y, num_repeats, holdoutRate,max_components);