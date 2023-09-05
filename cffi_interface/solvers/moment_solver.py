import numpy as np
import torch
from torch.utils.data import Dataset, DataLoader

    
class simulation_forecast:
    def __init__(
        self,
        all_moments_in,
        new_model,
        inputs_mean,
        inputs_std,
        updates_mean,
        updates_std
        
    ):
        self.all_moments_in = all_moments_in
        """ Assuming the moments to be arranged 
        as Lc, Nc,Lr, Nr """
        self.inputs_mean = inputs_mean
        self.inputs_std = inputs_std
        self.updates_mean = updates_mean
        self.updates_std = updates_std
        self.model = new_model
        self.moments_out = None
    
        #self.lo = all_moments_in[0]+ all_moments_in[2]
        
        

    def setup(self):

        self.all_moments_in = self.all_moments_in.astype(np.float32)
        self.all_moments_in = self.all_moments_in.reshape(-1,4) #Now we can process the whole batch
   
    def test(self):
        self.setup()
        self.create_input()
        self.model.eval()
        predictions_updates = self.model.test_step(torch.from_numpy(self.inputs))
        self.moment_calc(predictions_updates)

        self.moments_out = self.moments_out.astype(np.float64)

    # For Calculation of Moments
    def calc_mean(self, no_norm, means, stds):
        return (no_norm - means.reshape(-1,)) / stds.reshape(
            -1,
        )

    # For creation of inputs
    def create_input(self):
        divisor = self.all_moments_in[:,2] + self.all_moments_in[:,0]
        tau = np.divide( self.all_moments_in[:,2], divisor, where=divisor!=0 )

        xc = self.all_moments_in[:,0] / (self.all_moments_in[:,1] + 1e-8)
        self.lo_arr = self.all_moments_in[:,2] + self.all_moments_in[:,0]
        self.rm = np.full((self.lo_arr.shape),13e-6)
        self.nu = np.full((self.lo_arr.shape),1)

        self.model_params = np.concatenate((self.lo_arr.reshape(-1,1), 
                                            self.rm.reshape(-1,1),self.nu.reshape(-1,1)),
                                           axis=1)
        
        #self.model_params = np.asarray([self.lo,self.rm,self.nu])
        inputs = np.concatenate(
            (
                self.all_moments_in,
                tau.reshape(-1, 1),
                xc.reshape(-1, 1),
                self.model_params
            ),
            axis=1,
        )
        self.inputs = self.calc_mean(inputs, self.inputs_mean, self.inputs_std)
        
        self.inputs = np.float32(self.inputs)
        self.inputs_mean = np.float32(self.inputs_mean)
        self.inputs_std  = np.float32(self.inputs_std)
        # dataset =  self.my_dataset(self.inputs)

        # train_loader = DataLoader(dataset,shuffle=False,batch_size=inputs.shape[0])

    def check_preds(self):

        self.moments_out[:, 0] = (self.lo_arr) - self.moments_out[:, 2]
        self.moments_out[self.moments_out<0] = 0
          
        # manually set the output moments to zero when input moments were zero
        zero_ix = np.where( np.sum( np.abs( self.all_moments_in ), axis=1) < 1e-8 )[0]
        self.moments_out[zero_ix] = 0.0
    
    def moment_calc(self, predictions_updates):
        self.updates = (
            predictions_updates.detach().numpy() * self.updates_std
        ) + self.updates_mean
        
        self.moments_out = (self.all_moments_in[:,0:4] + (self.updates[:,:] * 20))
        self.check_preds()
        
