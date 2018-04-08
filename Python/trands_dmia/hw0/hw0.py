import numpy as np


def power_sum(l, r, p=1.0):
    return np.sum(np.arrange(l, r + 1, 1)**p)
    raise NotImplementedError


def solve_equation(a, b, c):
    if(a == 0 and b == 0):
        return None
    if(a == 0):
        return -c/b
    EPS = 10**(-10)
    D = b**2 - 4*a*c
    if(D > EPS):
        x1 = (-b - np.sqrt(D)) / (2*a)
        x2 = (-b + np.sqrt(D)) / (2*a)
        return [x1, x2]
    elif(abs(D) < EPS):
        return (-b) / (2*a)
    else:
        return np.inf
    raise NotImplementedError


def replace_outliers(x, std_mul=3.0):
    m = np.mean(x)
    s = np.std(x)
    return np.where(abs(x - m) > std_mul * s, m, x)
    raise NotImplementedError


def get_eigenvector(A, alpha):
    EPS = 10**(-6)
    s = np.linalg.eig(A)
    for i in range(len(A)):
        if abs(s[0][i] - alpha) < EPS:
            return s[1][i]
    raise NotImplementedError


def discrete_sampler(p):
    return np.random.choice(np.arange(len(p)),1,p = p)
    raise NotImplementedError


def gaussian_log_likelihood(x, mu=0.0, sigma=1.0):
    return np.sum(-np.log(sigma*np.sqrt(2*np.pi))-((x-mu)**2)/(2*(sigma**2)))
    raise NotImplementedError


def gradient_approx(f, x0, eps=1e-8):
    return (f(x0 + eps) - f(x0 - eps))/(2*eps)
    raise NotImplementedError


def gradient_method(f, x0, n_steps=1000, learning_rate=1e-2, eps=1e-8):
    x_k = x0
    for i in range(n_steps):
        x_k = x_k - gradient_approx(f, x_k, eps)*learning_rate
    return (f(x_k), x_k)
    raise NotImplementedError


def linear_regression_predict(w, b, X):
    return np.dot(X,w) + b
    raise NotImplementedError


def mean_squared_error(y_true, y_pred):
    return np.mean((y_true - y_pred)**2)
    raise NotImplementedError


def linear_regression_mse_gradient(w, b, X, y_true):
    d_w = np.matrix([np.array(-2 * np.dot(X.T[i], (y_true - linear_regression_predict(w, b, X))) / X.shape[0])[0][0] for i in range(len(w))]).T
    d_b = np.sum(-2 * (y_true - linear_regression_predict(w, b, X)) / X.shape[0])
    return (d_w, d_b)
    raise NotImplementedError


class LinearRegressor:
    def fit(self, X_train, y_train, n_steps=1000, learning_rate=1e-2, eps=1e-8):
        self.b = 1
        self.w = np.ones(X_train.shape[1])
        for i in range(n_steps):
            self.w = self.w - learning_rate*linear_regression_mse_gradient(self.w, self.b, X_train, y_train)[0]
            self.b = self.b - learning_rate*linear_regression_mse_gradient(self.w, self.b, X_train, y_train)[1]
        raise NotImplementedError
        return self


    def predict(self, X):
        return linear_regression_predict(self.w, self.b, X)



def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-x))


def sigmoid_der(x):
    return np.exp(-x) / ((1.0 + np.exp(-x))**2)
    raise NotImplementedError


def relu(x):
    return np.maximum(x, 0)


def relu_der(x):
    EPS = 10**(-6)
    alpha = 0
    if x > EPS:
        return 1
    elif x < EPS:
        return 0
    else:
        return aplha #alpha in [0,1] subdiff
    raise NotImplementedError



class MLPRegressor:
	"""
		simple dense neural network class for regression with mse loss. 
	"""
	def __init__(self, n_units=[32, 32], nonlinearity=relu):
		"""
			input: n_units - number of neurons for each hidden layer in neural network,
			       nonlinearity - activation function applied between hidden layers.
		"""
        
        self.n_units = n_units
        self.nonlinearity = nonlinearity


    def fit(self, X_train, y_train, n_steps=1000, learning_rate=1e-2, eps=1e-8):
        b = 1
        self.n_neur = [X_train.shape[1]]
        self.n_neur.extend(self.n_units)
        self.w = []
        for k in range(len(self.n_units)):
            for l in range(self.n_units[k]):
                self.w.append(np.random.uniform([-1/self.n_units[k],1/self.n_units[k],self.n_neur[k]]))
        l = len(X_train)
        M = X_train.shape[1]
        for step in range(n_steps):
            i = np.random.randint(l, size=1)
            x_i = X_train[i][:]
            for k in range(len(self.n_units)):
                u_i = np.zeros(self.n_units[k])
                a_i = np.zeros(M)
                e_i = np.zeros(M)
                e_i_h = np.zeros(self.n_units[k])
                for h in range(self.n_units[k]):
                    u_i[h] = relu(np.dot(x_i,self.w[k+h]))
                for m in range(M):
                    a_i[m] = relu(np.dot(u_i,self.w[h+m]))
                    e_i[m] = a_i[m] - y_i[m]
                Q_i = np.sum(e_i**2)
                for h in range(self.n_units[k]):
                    e_i_h[h] = np.sum(e_i*relu_der(self.w[k+h]))
                for h in range(self.n_units[k]):
                    for m in range(M):
                        self.w[h+m] = self.w[h+m] - learning_rate * e_i[m]*relu_der(u_h)
                for j in range(l):
                    for h in range(self.n_units[k]):
                        self.w[h+j] = self.w[h+j] - learning_rate * e_i[h]*relu_der(X_train[j][:])
            Q = Q*(l-1)/l+Q_i/l
                    
        
		"""
			input: object-feature matrix and targets.
			optimises mse w.r.t model parameters
			(you may use approximate gradient estimation)
		"""
        raise NotImplementedError


	def predict(self, X):
        return np.dot(X,self.w[-1])
		"""
			input: object-feature matrix
			returns MLP predictions in X
		"""
        raise NotImplementedError

