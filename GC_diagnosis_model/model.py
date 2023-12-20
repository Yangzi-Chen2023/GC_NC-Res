# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License.

import numpy as np
from sklearn.feature_selection import SelectFromModel
from sklearn.model_selection import RepeatedKFold
from sklearn.linear_model import Lasso

class Selection:
    def __init__(self, method, args):
        self.method = method
        self.args = args

    def fit_repeat_cv(self, X, y, metas, repeat=10, cv=5, threshold_ratio=0.5, random_state=10086):
        print("repeated corss validation")
        rkf = RepeatedKFold(n_splits=cv, n_repeats=repeat, random_state=random_state)
        res_dic = {}
        for train_index, test_index in rkf.split(X):
            X_here, y_here = X[train_index], y[train_index]
            cls_here = Lasso(alpha=self.args.alpha)
            cls_here.fit(X_here, y_here)
            sel_here = SelectFromModel(cls_here, prefit=True, max_features=self.args.k)
            mb_here = np.array(list(range(X.shape[1])))[sel_here.get_support()]
            if len(mb_here) >= self.args.k:
                print("over max_features", mb_here)
            for i in mb_here:
                if i in res_dic.keys():
                    res_dic[i] += 1
                else:
                    res_dic[i] = 1
        threshold = int(threshold_ratio * cv * repeat)
        mb = []
        print("all selected features with selected times:")
        sorted_res = sorted(res_dic.items(), key=lambda item: item[1], reverse=True)
        for k, v in sorted_res:
            print("feature_index: {}, meta nema: {}, selected times:{}".format(k, metas[k], v))
            if v >= threshold:
                mb.append(k)
        self.mb_ = np.array(sorted(mb))
        return self

    def fit(self, X, y=None):
        must = []
        cls = Lasso(alpha=self.args.alpha,normalize=True,max_iter=10000)
        cls.fit(X, y)
        imp = np.sort(np.abs(cls.coef_))[-self.args.k]
        print("imp:",imp)
        sel = SelectFromModel(cls, prefit=True, max_features=self.args.k)
        print("sel:",sel)
        self.mb_ = np.array(list(range(X.shape[1])))[sel.get_support()]
        print("self.mb_:",self.mb_)
        if len(self.mb_) >= self.args.k:
            print("over max_features", self.mb_)
        if isinstance(self.mb_,list):
            self.mb_ = sorted([i for i in self.mb_ if i not in must])
            print(self.mb_)
        return self
    
    def transform(self, X):
        if len(self.mb_)!=0:
            nw_X = X[:, self.mb_]
        else: return X
        return nw_X
