#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>

char cmd_res[4096];
#define die(e) do { fprintf(stderr, "%s\n", e); exit(EXIT_FAILURE); } while (0);

struct list
{
    void **data;
    int size;
    int mem;
    char *type;
};

struct list *create_list(char *type)
{
    struct list *inlist = malloc(sizeof(struct list));
    char *toadd = malloc(strlen(type));
    strcpy(toadd, type);
    inlist->type = toadd;
    inlist->size = 0;
    inlist->mem = 64;
    inlist->data = malloc(64);
    return inlist;
}

void realloc_check(struct list *inlist)
{
    if (inlist->mem <= (inlist->size * 8))
    {
        void **newdata = realloc(inlist->data, inlist->mem + 64);
        if (newdata == NULL)
        {
            printf("Failure to reallocate data");
            return;
        }
        inlist->data = newdata;
    }
}

int append_str(struct list *inlist, char *str)
{
    if (strcmp(inlist->type, "str"))
    {
        printf("Can only append %s, not str\n", inlist->type);
        return -1;
    }

    realloc_check(inlist);
    char *toadd = malloc(strlen(str));
    strcpy(toadd, str);
    inlist->data[(inlist->size)] = toadd;
    inlist->size = inlist->size + 1;
    return 0;
}

int append_int(struct list *inlist, int num)
{
    if (strcmp(inlist->type, "int"))
    {
        printf("Can only append %s, not int\n", inlist->type);
        return -1;
    }

    realloc_check(inlist);
    int *toadd = malloc(4);
    *toadd = num;
    inlist->data[(inlist->size)] = toadd;
    inlist->size = inlist->size + 1;
    return 0;
}

int append_float(struct list *inlist, float flt)
{
    if (strcmp(inlist->type, "float"))
    {
        printf("Can only append %s, not float\n", inlist->type);
        return -1;
    }

    realloc_check(inlist);
    float *toadd = malloc(4);
    *toadd = flt;
    inlist->data[(inlist->size)] = toadd;
    inlist->size = inlist->size + 1;
    return 0;
}

void *access_helper(struct list *inlist, int index)
{
    return inlist->data[index];
}

int access_int(struct list *inlist, int index)
{
    if (strcmp(inlist->type, "int"))
    {
        printf("Can only access %s, not int\n", inlist->type);
        return 0;
    }

    int *num = (int *)access_helper(inlist, index);
    if (num == NULL)
    {
        printf("Illegal index accessed\n");
        return 0;
    }
    return *num;
}


float access_float(struct list *inlist, int index)
{
    if (strcmp(inlist->type, "float"))
    {
        printf("Can only access %s, not float\n", inlist->type);
        return 0;
    }

    float *flt = (float *)access_helper(inlist, index);
    if (flt == NULL)
    {
        printf("Illegal index accessed\n");
        return 0;
    }
    return *flt;
}

char *access_str(struct list *inlist, int index)
{
    if (strcmp(inlist->type, "str"))
    {
        printf("Can only access %s, not str\n", inlist->type);
        return 0;
    }

    char *str = (char *)access_helper(inlist, index);
    if (str == NULL)
    {
        printf("Illegal index accessed\n");
        return NULL;
    }
    return str;
}

int assign_int(struct list *inlist, int index, int toAssign)
{
    if (strcmp(inlist->type, "int"))
    {
        printf("Can only access %s, not int\n", inlist->type);
        return 0;
    }

    if (index >= inlist->size || index < 0)
    {
        printf("Illegal index accessed\n");
        return 0;
    }

    *((int *)inlist->data[index]) = toAssign;

    return toAssign;
}

float assign_float(struct list *inlist, int index, float toAssign)
{
    if (strcmp(inlist->type, "float"))
    {
        printf("Can only access %s, not float\n", inlist->type);
        return 0;
    }

    if (index >= inlist->size || index < 0)
    {
        printf("Illegal index accessed\n");
        return 0;
    }

    *((float *)inlist->data[index]) = toAssign;

    return toAssign;
}

char *assign_str(struct list *inlist, int index, char *toAssign)
{
    if (strcmp(inlist->type, "str"))
    {
        printf("Can only access %s, not str\n", inlist->type);
        return 0;
    }

    if (index >= inlist->size || index < 0)
    {
        printf("Illegal index accessed\n");
        return 0;
    }

    inlist->data[index] = toAssign;

    return toAssign;
}

// Command

char* exec(char *command) {
  int link[2];
  pid_t pid;
  
  memset(cmd_res, 0, sizeof(cmd_res));

  if (pipe(link)==-1)
    die("pipe");

  if ((pid = fork()) == -1)
    die("fork");

  if(pid == 0) {

    dup2 (link[1], STDOUT_FILENO);
    close(link[0]);
    close(link[1]);
    system(command);
    exit(0);

  } else {

    close(link[1]);
    int nbytes = read(link[0], cmd_res, sizeof(cmd_res));
    printf("%.*s", nbytes, cmd_res);
    wait(NULL);

  }
  return cmd_res;
}
