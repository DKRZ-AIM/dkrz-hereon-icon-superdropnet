import os
import numpy as np
import pytorch_lightning as pl
import torch
import torch.nn as nn

class plNetwork(nn.Module):
    def __init__(
        self,
        act,
        n_layers=5,
        ns=200,
        out_features=4,
        depth=9,
        p=0.25,
        use_batch_norm=False,
        use_dropout=False,
    ):

        super().__init__()

        self.layers = []
        self.layer_size = []

        in_features = depth
        for i in range(n_layers):

            self.layers.append(nn.Linear(in_features, ns))
            # nn.init.kaiming_normal_(self.layers[i].weight, mode='fan_out')
            if use_batch_norm:
                self.layers.append(nn.BatchNorm1d(ns))
            in_features = ns

        self.layers.append(nn.Linear(ns, out_features))
        self.layers = nn.ModuleList(self.layers)
        self.dropout = nn.Dropout(p)
        self.activation = act
        self.n_layers = n_layers
        self.use_dropout = use_dropout

    def _forward_impl(self, x):
        for i in range(self.n_layers - 1):
            x = self.activation(self.layers[i](x))
            if self.use_dropout:
                x = self.dropout(x)

        x = self.layers[-1](x)

        return x

    def forward(self, x):
        return self._forward_impl(x)

class LightningModel(pl.LightningModule):
    def __init__(
        self,
        updates_mean,
        updates_std,
        inputs_mean,
        inputs_std,
        save_dir=None,
        batch_size=256,
        beta=0.35,
        learning_rate=2e-4,
        act=nn.ReLU(),
        loss_func=None,
        n_layers=5,
        ns=200,
        depth=9,
        p=0.25,
        mass_cons_updates=True,
        loss_absolute=True,
        mass_cons_moments=True,
        hard_constraints_updates=True,
        hard_constraints_moments=False,
        multi_step=False,
        step_size=1,
        moment_scheme=2,
        plot_while_training=False,
        plot_all_moments=True,
        use_batch_norm=False,
        use_dropout=False,
        single_sim_num=None,
        avg_dataloader=False,
        pretrained_path=None
    ):
        super().__init__()
        self.moment_scheme = moment_scheme
        self.out_features = moment_scheme * 2
        self.lr = learning_rate
        self.loss_func = loss_func
        self.beta = beta
        self.batch_size = batch_size

        self.loss_absolute = loss_absolute

        """ Using the following only for multi-step training"""
        self.hard_constraints_updates = hard_constraints_updates
        self.hard_constraints_moments = hard_constraints_moments
        self.mass_cons_updates = mass_cons_updates  # Serves no purpose now
        self.mass_cons_moments = mass_cons_moments

        self.multi_step = multi_step
        self.step_size = step_size
        self.num_workers = 48
        self.plot_while_training = plot_while_training
        self.plot_all_moments = plot_all_moments
        self.single_sim_num = single_sim_num
        self.avg_dataloader = avg_dataloader
        self.save_hyperparameters()

        self.updates_std = torch.from_numpy(updates_std).float().to("cpu")
        self.updates_mean = torch.from_numpy(updates_mean).float().to("cpu")
        self.inputs_mean = torch.from_numpy(inputs_mean).float().to("cpu")
        self.inputs_std = torch.from_numpy(inputs_std).float().to("cpu")

        # Some plotting stuff
        self.color = ["#26235b", "#bc473a", "#812878", "#f69824"]
        self.var = ["Lc", "Nc", "Lr", "Nr"]
        self.pretrained_path = pretrained_path
        self.model = self.initialization_model(
            act,
            n_layers,
            ns,
            self.out_features,
            depth,
            p,
            use_batch_norm,
            use_dropout,
            save_dir,
            pretrained_path,
        )

    @staticmethod
    def initialization_model(
        act,
        n_layers,
        ns,
        out_features,
        depth,
        p,
        use_batch_norm,
        use_dropout,
        save_dir,
        pretrained_path,
    ):
        model = plNetwork(
            act, n_layers, ns, out_features, depth, p, use_batch_norm, use_dropout
        )
        model.train()
        return model

    def forward(self):
        self.updates = self.model(self.x)
        self.norm_obj = normalizer(
            self.updates,
            self.x,
            self.y,
            self.updates_mean,
            self.updates_std,
            self.inputs_mean,
            self.inputs_std,
            self.device,
            self.hard_constraints_updates,
            self.hard_constraints_moments,
        )
        (
            self.real_x,
            self.real_y,
            self.pred_moment,
            self.pred_moment_norm,
        ) = self.norm_obj.calc_preds()
        self.pred_moment, self.pred_moment_norm = self.norm_obj.set_constraints()


    def test_step(self, initial_moments):

        """For moment-wise evaluation as used for ODE solve"""
        with torch.no_grad():
            preds = self.model(initial_moments.float())
        return preds
