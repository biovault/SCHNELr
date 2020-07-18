#pylint:disable-all

from scipy.sparse import csc_matrix, lil_matrix, coo_matrix
import struct
import time
import numpy as _np
import warnings
import igraph as ig
import leidenalg

class HSNE:
    # HSNE hierarchy object
    # Supports slicing and looping
    # Contains the scales of which the hierarchy is built
    def __init__(self, num_scales):
        # Number of scales in hierarchy including datascale
        self.num_scales = num_scales
        # Scales which are at index 0 a datascale and the rest are subscales
        self.scales = [None] * num_scales
        self._index = -1

    def __str__(self):
        return "HSNE hierarchy with %i scales" % self.num_scales

    def __getitem__(self, index):
        return self.scales[index]

    def __setitem__(self, index, value):
        self.scales[index] = value

    def __iter__(self):
        return self

    def __next__(self):
        if self._index == self.num_scales - 1:
            self._index = -1
            raise StopIteration
        self._index += 1
        return self.scales[self._index]

    def get_topscale(self):
        return self.scales[0]

    def get_datascale_mappings(self, scalenumber):
        if scalenumber <= 0:
            raise ValueError("Can't generate mapping for complete dataset, only scales get clustered")
        if scalenumber > self.num_scales:
            raise ValueError("Scale doesn't exist, object has %i scales" % self.num_scales)
        maps = None
        for scale in self.scales[1:scalenumber + 1]:  # Don't include datascale
            if maps is None:
                maps = dict.fromkeys(range(self.get_topscale().size))
                for idx, value in enumerate(scale.best_representatives):
                    maps[idx] = value
            else:
                for key in maps:
                    maps[key] = scale.best_representatives[maps[key]]
        return maps

    def get_map_by_cluster(self, scalenumber, clustering):
        if len(clustering) != self.scales[int(scalenumber)].area_of_influence.shape[1]:
            raise ValueError("Number of labels does not match number of landmarks in scale")
        if scalenumber <= 0:
            raise ValueError("Can't generate mapping for complete dataset, only scales get clustered")
        for scale in self.scales[int(scalenumber):0:-1]:  # Don't include datascale
            new_aoi = lil_matrix((scale.area_of_influence.shape[0],
                                  len(set(clustering))))
            for i, label in enumerate(_np.unique(clustering)):
                new_aoi[:, i] = scale.area_of_influence * [[1] if label == x else [0] for x in clustering]
            clustering = csc_matrix(new_aoi).argmax(axis=1).A1
        return clustering

    def cluster_scale(self, scalenumber, prop_method='cluster'):

        '''Cluster the given scale using Louvain community detection'''
        if scalenumber == 0:
            warnings.warn("Warning: You are about to cluster the full dataset, this might take a very long time")
            return self.run_louvain(scalenumber)
        elif scalenumber >= self.num_scales:
            raise ValueError("Scale doesn't exist, object has %i scales" % self.num_scales)
        if prop_method == 'cluster':
            membership = self.run_louvain(scalenumber)
            return self.get_map_by_cluster(scalenumber, membership)
        elif prop_method == 'label':
            membership = self.run_louvain(scalenumber)
            mapping = self.get_datascale_mappings(scalenumber)
            return _np.asarray(membership)[list(mapping.values())]


        else:
            raise ValueError("Invalid method, options are 'label' or 'cluster'")

    def run_louvain(self, scalenumber):
        sources, targets = self.scales[int(scalenumber)].tmatrix.nonzero()
        edgelist = list(zip(sources.tolist(), targets.tolist()))
        G = ig.Graph(edgelist)
        G.es['weight'] = self.scales[int(scalenumber)].tmatrix.data
        return leidenalg.find_partition(G, leidenalg.ModularityVertexPartition, weights=G.es['weight']).membership


class DataScale:
    # HSNE datalevel object contains only the transition matrix
    def __init__(self, num_scales, tmatrix=None):
        self.tmatrix = tmatrix
        self.size = tmatrix.shape[0]
        self.datapoints = [idx for idx in range(self.size)]
        self.scalenum = 0
        self.num_scales = num_scales

    def __str__(self):
        return "HSNE datascale %i with %i datapoints" % (self.scalenum, self.size)


# HSNE L(s-x) scales contain many mappings


class SubScale:
    def __init__(self, scalenum, num_scales, tmatrix,
                 lm_to_original, lm_to_previous, lm_weights, previous_to_current, area_of_influence):
        # The transition matrix / graph
        self.tmatrix = tmatrix
        # Number of landmarks in scale
        self.size = tmatrix.shape[0]
        self.datapoints = [idx for idx in range(self.size)]
        # Scalenumber
        self.scalenum = scalenum
        # NUmber of scales in hierarchy
        self.num_scales = num_scales
        # Which landmark is which original datapoint
        self.lm_to_original = lm_to_original
        # Which landmark is which datapoint in the previous scale (reduntant
        # with lm_to_original on scale 1.
        self.lm_to_previous = lm_to_previous
        # LM Weights is equal to the sum of AOI columns
        self.lm_weights = lm_weights
        # Which landmark on previous scale is landmark on current scale
        self.previous_to_current = previous_to_current
        # Comes in as S x S where all columns > S-1 are 0's
        # Cast to csc to efficiently slice all columns outside range S-1
        self.area_of_influence = csc_matrix(area_of_influence)[:, :self.size]
        # The best representative landmark in scale S  for each point in scale S-1 is
        # the node that was visited most often e.g. has the highest value in its row
        # in area_of_influence.
        self.best_representatives = self.area_of_influence.argmax(axis=1).A1

    def __str__(self):
        return "HSNE subscale %i with %i datapoints" % (self.scalenum, self.size)



class HSNE_parser:
    def read_uint_vector(handle):
        '''
        Read unsigned int vector from HDI binary file
        :param handle: _io.BufferedReader (object result from calling native Python open() )
        :return: list
        '''
        vectorlength = struct.unpack('i', handle.read(4))[0]
        vector = list(struct.unpack('i' * vectorlength, handle.read(4 * vectorlength)))
        return vector
    
    
    def read_scalar_vector(handle):
        '''
        Read float vector from HDI binary file
        :param handle: _io.BufferedReader (object result from calling native Python open() )
        :return: list
        '''
        vectorlength = struct.unpack('i', handle.read(4))[0]
        vector = list(struct.unpack('f' * vectorlength, handle.read(4 * vectorlength)))
        return vector
    
    
    def read_HSNE_binary(filename, verbose=True):
        '''
        Read HSNE binary from file and construct HSNE object with top- and subscales
        :param filename: str, file to read
        :param verbose: bool, controls verbosity of parser
        :return: HSNE object
        '''
        logger = Logger(verbose)
        longtic = time.time()
        with open(filename, 'rb') as handle:
            _, _ = struct.unpack('ff', handle.read(8))  # Never used
            numscales = int(struct.unpack('f', handle.read(4))[0])
            scalesize = int(struct.unpack('f', handle.read(4))[0])
            logger.log("Number of scales %i" % numscales)
            hierarchy = HSNE(numscales)
            logger.log("Start reading first scale of size %i" % scalesize)
            tmatrix = HSNE_parser.read_sparse_matrix(handle)
            logger.log("Done reading first scale..")
            hierarchy[0] = DataScale(num_scales=numscales, tmatrix=tmatrix)
            for i in range(1, numscales):
                hierarchy[i] = HSNE_parser.build_subscale(handle, i, numscales, logger)
            print('Total time spent parsing hierarchy and building objects: %f' % (time.time() - longtic))
            return hierarchy
    
    
    def read_sparse_matrix(handle):
        cols = []
        rows = []
        weights = []
        numrows = struct.unpack('i' , handle.read(4))[0]
        shape = numrows
        for rownum in range(numrows):
            rowlen = struct.unpack('i', handle.read(4))[0]
            row = list(struct.unpack("if" * rowlen, handle.read(8 * rowlen)))
            cols += row[::2]
            weights += row[1::2]
            rows += [rownum] * rowlen
        return coo_matrix((weights, (rows, cols)), shape=(shape, shape))
    
    
    def build_subscale(handle, i, numscales, logger):
        '''
        :param handle: _io.BufferedReader (object result from calling native Python open() )
        :param i: int, current scale
        :param numscales: total number of scales
        :param logger: Logger object
        :return: Subscale
        '''
        logger.log("\nNext scale: %i" % i)
        scalesize = int(struct.unpack('f', handle.read(4))[0])
        logger.log("Scale size: %i" % scalesize)
        logger.log("Reading transmatrix..")
        tmatrix = HSNE_parser.read_sparse_matrix(handle)
        logger.log("Reading landmarks of scale to original data..")
        lm_to_original = HSNE_parser.read_uint_vector(handle)
        logger.log("Reading landmarks to previous scale..")
        lm_to_previous = HSNE_parser.read_uint_vector(handle)
        logger.log("Reading landmark weights..")
        lm_weights = HSNE_parser.read_scalar_vector(handle)
        logger.log("Reading previous scale to current scale..")
        previous_to_current = HSNE_parser.read_uint_vector(handle)
        logger.log("Reading area of influence..")
        area_of_influence = HSNE_parser.read_sparse_matrix(handle)
        subscale = SubScale(scalenum=i,
                            num_scales=numscales,
                            tmatrix=tmatrix,
                            lm_to_original=lm_to_original,
                            lm_to_previous=lm_to_previous,
                            lm_weights=lm_weights,
                            previous_to_current=previous_to_current,
                            area_of_influence=area_of_influence
                            )
        return subscale
        

class Logger:
    # Message printer that can be turned off by initializing it with enabled=False
    def __init__(self, enabled):
        self._enabled = enabled

    def enable(self):
        self._enabled = True

    def disable(self):
        self._enabled = False

    def log(self, message):
        if self._enabled:
            print(message)


#if __name__ == "__main__":
#    import sys
#    read_HSNE_binary(sys.argv[1], verbose=True)
