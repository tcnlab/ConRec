clear all
close all
dt = 0.01;
t = 0:dt:25;
lambda = 10; % boundary
sigma = 0; % time shift
q_vec = [0.1:0.1:0.9]; % vector of quantiles
%% drift
%mu = 1; %drift rate
mu = 0.8:0.2:1.6; %drift rate

%linecolors = gray(length(mu));
%linecolors(find(linecolors==1)) = 0.85; % avoid white color
linecolors = [0, 0, 1, .4; 0, 0, 1, .55; 0, 0, 1, .7; 0, 0, 1, .85; 0, 0, 1, 1];
glinecolors = .85*gray(length(q_vec));

for i = 1:length(mu) 
    y_wald(i,:) = wald_dist(t, lambda, sigma, mu(i));
    figure(1), hold on, subplot(1,3,1) 
    plot(t+sigma,y_wald(i,:),'color',linecolors(i,:))
    set(gca,'xlim',[0 t(end)])
    set(gca,'XTick',[])
    set(gca,'YTick',[])
    xlabel('Response Time')
    ylabel('PDF')
    hold on, 
    
    figure(1), hold on, subplot(1,3,2)

    cdf_data = cumsum(y_wald(i,~isnan(y_wald(i,:))));
    plot(t(~isnan(y_wald(i,:)))+sigma,cdf_data,'color',linecolors(i,:))
    set(gca,'xlim',[0 t(end)])
    set(gca,'ylim',[0 1])
    set(gca,'XTick',[])
    set(gca,'YTick',[])
    xlabel('Response Time')
    ylabel('Cumulative Probability')

    quant_ind_drift(i,:) = quantile_all_cdf(cdf_data,q_vec);
    
end


figure(1),subplot(1,3,3)
for i = 1:length(q_vec) % to number of quantiles
    lm = fitlm([1:length(quant_ind_drift(:,i))], flipud(quant_ind_drift(:,i)));
    slope_drift(i) = lm.Coefficients.Estimate(2);
    intercept_drift(i) = lm.Coefficients.Estimate(1);    
    hold on, plot(i,slope_drift(i)*dt,'ko', 'color', glinecolors(i,:))
    %hold on, plot(i,intercept_drift(i),'bo')
end
plot(slope_drift*dt,'k','color','blue')
%plot(intercept_drift,'b')
%set(gca,'ylim',[min(slope_drift)-10 max(slope_drift)+10])
ylabel('Lag Modulation Factor')
xlabel('Quantile')
set(gca,'XTick',[])
set(gca,'YTick',[])


%% shift
sigma = 1:3:13; % time shift
%linecolors = gray(length(mu));
linecolors = [1, 0, 0, .4; 1, 0, 0, .55; 1, 0, 0, .7; 1, 0, 0, .85; 1, 0, 0, 1];

mu = 2; %drift rate
for i = 1:length(sigma) 
    y_wald(i,:) = wald_dist(t, lambda, sigma(i), mu);
    figure(2), hold on, subplot(1,3,1) 
    plot(t+sigma(i),y_wald(i,:),'color',linecolors(i,:))
    set(gca,'xlim',[0 t(end)])
    xlabel('Response Time')
    ylabel('PDF')
    set(gca,'XTick',[])
    set(gca,'YTick',[])
    figure(2), hold on, subplot(1,3,2) 
    cdf_data = cumsum(y_wald(i,~isnan(y_wald(i,:))));
    plot(t(~isnan(y_wald(i,:)))+sigma(i),cdf_data,'color',linecolors(i,:))
    set(gca,'xlim',[0 t(end)])
    set(gca,'ylim',[0 1])
    set(gca,'XTick',[])
    set(gca,'YTick',[])
    xlabel('Response Time')
    ylabel('Cumulative Probability')
    
    quant_ind_shift(i,:) = quantile_all_cdf(cdf_data,q_vec) + sigma(i)/dt;
    
end

figure(2), subplot(1,3,3)
for i = 1:length(q_vec) % to number of quantiles
    lm = fitlm([1:length(quant_ind_shift(:,i))], quant_ind_shift(:,i));
    slope_shift(i) = lm.Coefficients.Estimate(2);
    intercept_shift(i) = lm.Coefficients.Estimate(1);
    hold on, plot(i,slope_shift(i)*dt,'ko', 'color',glinecolors(i,:))
    %hold on, plot(i,intercept_shift(i),'bo')
end
plot(slope_shift*dt,'k', 'color', 'red')
%plot(intercept_shift,'b')
%set(gca,'ylim',[min(slope_shift)-10 max(slope_shift)+10])
ylabel('Lag Modulation Factor')
xlabel('Quantile')
%legend('Slope', 'Intercept')
ylim([2, 4])
set(gca,'XTick',[])
    set(gca,'YTick',[])
linecolors = cool(length(q_vec));
