import numpy as np
import torch


class simulation_forecast:
    def __init__(
        self,
        moments_in,
        new_model,
        inputs_mean,
        inputs_std,
        
    ):
        self.moments_in = moments_in
        """ Assuming the moments to be arranged 
        as Lc, Nc,Lr, Nr """
        self.inputs_mean = inputs_mean
        self.inputs_std = inputs_std
        self.model = new_model
        self.moments_out = self.moments_out
        self.nu = 1
        self.rm = 13e-6
        self.lo = self.moments_in[0]+ self.moments_in[2]
        
        

    def setup(self):

        self.moments_in = self.moments_in.astype(np.float32)
   
    def test(self):
        self.setup()
        self.create_input()
        self.preds = self.model.test_step(torch.from_numpy(self.inputs))
        self.moments_out = (
            self.preds * self.inputs_std[:4]
        ) + self.inputs_mean [:4]
        
        self.check_preds()

      

    # For Calculation of Moments
    def calc_mean(self, no_norm, means, stds):
        return (no_norm - means.reshape(-1,)) / stds.reshape(
            -1,
        )

    # For creation of inputs
    def create_input(self):
        tau = self.moments_in[2] / (self.moments_in[2] + self.moments_in[0])

        xc = self.moments_in[0] / (self.moments_in[1] + 1e-8)
        self.model_params = np.asarray([self.lo,self.rm,self.nu])
        inputs = np.concatenate(
            (
                self.moments_in.reshape(1, -1),
                tau.reshape(1, -1),
                xc.reshape(1, -1),
                self.model_params.reshape(1, -1),
            ),
            axis=1,
        )
        self.inputs = self.calc_mean(inputs, self.inputs_mean, self.inputs_std)
        self.inputs = np.float32(self.inputs)

    def check_preds(self):

        if self.moments_out[0, 0] < 0:
            self.moments_out[0, 0] = 0

        if self.moments_out[0, 2] < 0:
            self.moments_out[0, 2] = 0

        if self.moments_out[0, 2] > self.model_params[0]:
            self.moments_out[0, 2] = self.model_params[0]

        if self.moments_out[0, 1] < 0:
            self.moments_out[0, 1] = 0

        if self.moments_out[0, 3] < 0:
            self.moments_out[0, 3] = 0

        self.moments_out[:, 0] = self.lo - self.moments_out[:, 2]

  
        

