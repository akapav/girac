#include <gtk/gtk.h>
#include <stdio.h>

GClosure *closure = NULL;

int(*bar)(int) = NULL;

//
void pif(int(*f)(int))
{ printf("f: %d\n", f(1)); }

//
int cbck(GtkWidget *wdg, GdkEventAny *evt, gpointer p_) {
  int *p = (int*)p_;
  printf("callback: %d\n", (*p)++);
  return *p != 3;
}

void cbck_notify(gpointer p_, GClosure *closure) { 
  int *p = (int*)p_;
  printf("notify: %d\n", *p);
}

int cnt = 0;
void foo_closure()
{ closure = g_cclosure_new(G_CALLBACK(cbck), &cnt, cbck_notify); }

//
typedef struct {
  GClosure closure;
  int foo;
  int bar;
} Tito;

static void tito_finalize(gpointer data, GClosure *closure)
{ puts("ciao tito"); }

Tito *tito_new(gpointer data) {
  GClosure *closure = g_closure_new_simple(sizeof(Tito), data);
  Tito *tito = (Tito*)closure;

  g_closure_add_finalize_notifier(closure, data, tito_finalize);
  return tito;
}

void marshal(GClosure *closure,
	     GValue *return_value,
	     guint n_param_values,
	     const GValue *param_values,
	     gpointer invocation_hint,
	     gpointer marshal_data) {
  puts("marshaled");
  puts((char*)marshal_data);
  static int foo = 1;
  g_value_set_boolean(return_value, (gboolean)foo--);
}

void tito_closure() {
  closure = (GClosure*)tito_new(NULL);
  g_closure_set_meta_marshal(closure, "foo", marshal);
}

//
void half_racket(GClosureMarshal* marshal) {
  closure = (GClosure*)tito_new(NULL);
  g_closure_set_marshal(closure, *marshal);
}

//
void run(void) {
  GtkWidget *window;
  gtk_init(0, NULL);
  if(!closure) {
    puts("no closure!");
    return;
  }

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  g_signal_connect_closure(window, "delete_event", closure, 0);
  g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), 0);
  gtk_widget_show(window);
  gtk_main();
}
