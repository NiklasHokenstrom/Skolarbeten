/**
 * Binary search tree implementation
 *
 * Copyright (c) 2013 the authors listed at the following URL, and/or
 * the authors of referenced articles or incorporated external code:
 * http://en.literateprograms.org/Binary_search_tree_(C)?action=history&offset=20121127201818
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Retrieved from: http://en.literateprograms.org/Binary_search_tree_(C)?oldid=18734
 * Modified: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 */


/***********************************************************/
/* NOTE: You can modify/add any piece of code that will    */
/* make your algorithm work                                */
/***********************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "bst.h"

pthread_mutex_t mutex;

/**
 * Searches for the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be search for
 * @return           the node containg the data
 */

static void lock_lock(struct bst_node** node) {
    if (!((*node) == NULL)) {
//        puts("Tar ett lock!\n");
        pthread_mutex_lock(&((*node)->lock));
    }
    
}

static void unlock_lock(struct bst_node** node) {
    if (!((*node) == NULL)) {
//        puts("Ger ett lock!\n");
        pthread_mutex_unlock(&((*node)->lock));
    }
    
}

struct bst_node** search(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: For the Step 2 you will have to make this function thread-safe */
    lock_lock(root);
    struct bst_node** node = root;
    struct bst_node** old_node;
    //lock_lock(node);
    while (*node != NULL) {
        int compare_result = compare(data, (*node)->data);
        if (compare_result < 0) {
            old_node = node;
            lock_lock (&((*node)->left));
            node = &(*node)->left;
            unlock_lock(old_node);
        }
        else if (compare_result > 0) {
            old_node = node;
            lock_lock (&((*node)->right));
            node = &(*node)->right;
            unlock_lock(old_node);
        }
        else
            break;
    }
    //unlock_lock(node);
    return node;
}


/**
 * Deletes the requested node.
 *
 * @param node       node to be deleted
 */
static void
node_delete_aux(struct bst_node** node)
{
    /* TODO: For Step 2 you will have to make this function thread-safe */
    struct bst_node* old_node = *node;
    lock_lock(&(*node)->left);
    struct bst_node** pred = &(*node)->left;
    //lock_lock(pred);
    if ((*node)->left == NULL) {
        lock_lock(&((*node)->right));
        *node = (*node)->right;
        free_node(old_node);
        unlock_lock(node);
    } else if ((*node)->right == NULL) {
        //lock_lock(&((*node)->left));
        *node = (*node)->left;
        free_node(old_node);
        unlock_lock(node);
    } else {
        //struct bst_node** pred = &(*node)->left;
        //lock_lock(pred);
        struct bst_node** old_pred;
	while ((*pred)->right != NULL) {
            old_pred = pred;
            lock_lock(&((*pred)->right));
	    pred = &(*pred)->right;
            unlock_lock(old_pred);
	}

	/* Swap values */
	void* temp = (*pred)->data;
	(*pred)->data = (*node)->data;
	(*node)->data = temp;
       
        //lock_lock (pred);
        unlock_lock (node);
        
	
        node_delete_aux(pred);
    }    
}

/**
 * Deletes the node which points to the requested data.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = search(root, compare, data);

    if (*node == NULL)
        return -1;

    node_delete_aux(node);
    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_cg(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: Fill-in the body of this function */
    pthread_mutex_lock (&mutex);
    struct bst_node** node = NULL;
    node = search(root, compare, data);

    if (node == NULL) {
    pthread_mutex_unlock (&mutex);
        return -1;
    }
    
    node_delete_aux(node);
    pthread_mutex_unlock (&mutex);
    return 0;
}

/**
 * Deletes the node which points to the requested data.
 *
 * Should be safe when called in parallel with other threads that
 * might call the same functions. Uses fine grained locking.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be deleted
 * @return           1 if data is not found, 0 otherwise
 */
int
node_delete_ts_fg(struct bst_node** root, comparator compare, void* data)
{
    /* TODO: Fill-in the body of this function */
    struct bst_node** node = NULL;
    node = search(root, compare, data);
    //lock_lock(node);
    if (node == NULL)
        return -1;
    
    node_delete_aux(node);
    return 0;
}


/**
 * Allocate resources and initialize a BST.
 *
 * @return           root of the BST
 */
struct bst_node **
tree_init(void)
{
    struct bst_node** root = malloc(sizeof(*root));
    if (root == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    }
    *root = NULL;

    /* TODO: Initialize any global variables you use for the BST */
    pthread_mutex_init(&mutex, NULL);
    return root;
}

/**
 * Remove resources for the tree.
 *
 * @param root       root of the tree
 */
void
tree_fini(struct bst_node ** root)
{
    /* TODO: Free any global variables you used for the BST */
    pthread_mutex_destroy(&mutex);
    if (root != NULL)
        free(root);
}


/**
 * Inserts a new node with the requested data if not already in the tree.
 *
 * @param root       root of the tree
 * @param comparator function used to compare nodes
 * @param data       pointer to the data to be inserted
 * @return           1 if data is in the BST already, 0 otherwise
 */
int
node_insert(struct bst_node** root, comparator compare, void* data)
{
    struct bst_node** node = search(root, compare, data);
     unlock_lock(node);
    if (*node == NULL) {
        *node = new_node(data);
        return 0;
    } else
        return 1;
}


/**
 * Creates a new node with the requested data.
 *
 * @param data       pointer to the data pointed be the new node
 */
struct bst_node* 
new_node(void* data)
{
    struct bst_node* node = malloc(sizeof(struct bst_node));
    if (node == NULL) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    } else {
        /* TODO: Initialize any per node variables you use for the BST */
        pthread_mutex_init(&node->lock, NULL);
        node->left = NULL;
        node->right = NULL;
        node->data = data;
    }

    return node;
}


/**
 * Deletes a node.
 *
 * @param node       node to be freed
 */
void
free_node(struct bst_node* node) 
{
    if (node == NULL)
        fprintf(stderr, "Invalid node\n");
    else {
        /* TODO: Finalize any per node variables you use for the BST */
        pthread_mutex_unlock(&node->lock);
        pthread_mutex_destroy(&node->lock);
        free(node);
    }
}


/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * c-file-style: "stroustrup"
 * End:
 */
